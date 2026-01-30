/**
 * Defines a `Dsymbol` representing an aggregate, which is a `struct`, `union` or `class`.
 *
 * Specification: $(LINK2 https://dlang.org/spec/struct.html, Structs, Unions),
 *                $(LINK2 https://dlang.org/spec/class.html, Class).
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/aggregate.d, _aggregate.d)
 * Documentation:  https://dlang.org/phobos/dmd_aggregate.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/aggregate.d
 */

module dmd.aggregate;

import core.stdc.stdio;
import core.checkedint;

import dmd.aliasthis;
import dmd.arraytypes;
import dmd.astenums;
import dmd.attrib;
import dmd.declaration;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem : determineSize;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.func;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.tokens;
import dmd.visitor;

/**
 * The ClassKind enum is used in AggregateDeclaration AST nodes to
 * specify the linkage type of the struct/class/interface or if it
 * is an anonymous class. If the class is anonymous it is also
 * considered to be a D class.
 */
enum ClassKind : ubyte
{
    /// the aggregate is a d(efault) class
    d,
    /// the aggregate is a C++ struct/class/interface
    cpp,
    /// the aggregate is an Objective-C class/interface
    objc,
    /// the aggregate is a C struct
    c,
}

/**
 * Give a nice string for a class kind for error messages
 * Params:
 *     c = class kind
 * Returns:
 *     0-terminated string for `c`
 */
const(char)* ClassKindToChars(ClassKind c) @safe
{
    final switch (c)
    {
        case ClassKind.d:
            return "D";
        case ClassKind.cpp:
            return "C++";
        case ClassKind.objc:
            return "Objective-C";
        case ClassKind.c:
            return "C";
    }
}

/**
 * If an aggregate has a pargma(mangle, ...) this holds the information
 * to mangle.
 */
struct MangleOverride
{
    Dsymbol agg;   // The symbol to copy template parameters from (if any)
    Identifier id; // the name to override the aggregate's with, defaults to agg.ident
}

/***********************************************************
 * Abstract aggregate as a common ancestor for Class- and StructDeclaration.
 */
extern (C++) abstract class AggregateDeclaration : ScopeDsymbol
{
    Type type;                  ///
    STC storage_class;          ///
    uint structsize;            /// size of struct
    uint alignsize;             /// size of struct for alignment purposes
    VarDeclarations fields;     /// VarDeclaration fields including flattened AnonDeclaration members
    Dsymbol deferred;           /// any deferred semantic2() or semantic3() symbol

    /// specifies whether this is a D, C++, Objective-C or anonymous struct/class/interface
    ClassKind classKind;
    /// Specify whether to mangle the aggregate as a `class` or a `struct`
    /// This information is used by the MSVC mangler
    /// Only valid for class and struct. TODO: Merge with ClassKind ?
    CPPMANGLE cppmangle;

    /// overridden symbol with pragma(mangle, "...") if not null
    MangleOverride* pMangleOverride;

    /**
     * !=null if is nested
     * pointing to the dsymbol that directly enclosing it.
     * 1. The function that enclosing it (nested struct and class)
     * 2. The class that enclosing it (nested class only)
     * 3. If enclosing aggregate is template, its enclosing dsymbol.
     *
     * See AggregateDeclaraton::makeNested for the details.
     */
    Dsymbol enclosing;

    VarDeclaration vthis;   /// 'this' parameter if this aggregate is nested
    VarDeclaration vthis2;  /// 'this' parameter if this aggregate is a template and is nested

    // Special member functions
    FuncDeclarations invs;  /// Array of invariants
    FuncDeclaration inv;    /// Merged invariant calling all members of invs

    /// CtorDeclaration or TemplateDeclaration
    Dsymbol ctor;

    /// default constructor - should have no arguments, because
    /// it would be stored in TypeInfo_Class.defaultConstructor
    CtorDeclaration defaultCtor;

    AliasThis aliasthis;    /// forward unresolved lookups to aliasthis

    DtorDeclarations userDtors; /// user-defined destructors (`~this()`) - mixins can yield multiple ones
    DtorDeclaration aggrDtor;   /// aggregate destructor calling userDtors and fieldDtor (and base class aggregate dtor for C++ classes)
    DtorDeclaration dtor;       /// the aggregate destructor exposed as `__xdtor` alias
                                /// (same as aggrDtor, except for C++ classes with virtual dtor on Windows)
    DtorDeclaration tidtor;     /// aggregate destructor used in TypeInfo (must have extern(D) ABI)
    DtorDeclaration fieldDtor;  /// function destructing (non-inherited) fields

    Expression getRTInfo;   /// pointer to GC info generated by object.RTInfo(this)

    ///
    Visibility visibility;
    bool noDefaultCtor;             /// no default construction
    bool disableNew;                /// disallow allocations using `new`
    Sizeok sizeok = Sizeok.none;    /// set when structsize contains valid data

    final extern (D) this(Loc loc, Identifier id)
    {
        super(loc, id);
        visibility = Visibility(Visibility.Kind.public_);
    }

    /***************************************
     * Create a new scope from sc.
     * semantic, semantic2 and semantic3 will use this for aggregate members.
     */
    Scope* newScope(Scope* sc)
    {
        auto sc2 = sc.push(this);
        sc2.stc &= STC.flowThruAggregate;
        sc2.parent = this;
        sc2.inunion = isUnionDeclaration();
        sc2.visibility = Visibility(Visibility.Kind.public_);
        sc2.explicitVisibility = false;
        sc2.aligndecl = null;
        sc2.userAttribDecl = null;
        sc2.namespace = null;
        return sc2;
    }

    /***************************************
     * Returns:
     *      The total number of fields minus the number of hidden fields.
     */
    extern (D) final size_t nonHiddenFields()
    {
        return fields.length - isNested() - (vthis2 !is null);
    }

    override final ulong size(Loc loc)
    {
        //printf("+AggregateDeclaration::size() %s, scope = %p, sizeok = %d\n", toChars(), _scope, sizeok);
        bool ok = determineSize(this, loc);
        //printf("-AggregateDeclaration::size() %s, scope = %p, sizeok = %d\n", toChars(), _scope, sizeok);
        return ok ? structsize : SIZE_INVALID;
    }


    override final Type getType()
    {
        /* Apply storage classes to forward references. (Issue 22254)
         * Note: Avoid interfaces for now. Implementing qualifiers on interface
         * definitions exposed some issues in their TypeInfo generation in DMD.
         * Related PR: https://github.com/dlang/dmd/pull/13312
         */
        if (semanticRun == PASS.initial && !isInterfaceDeclaration())
        {
            auto stc = storage_class;
            if (_scope)
                stc |= _scope.stc;
            type = type.addSTC(stc);
        }
        return type;
    }

    // is aggregate deprecated?
    override final bool isDeprecated() const
    {
        return !!(this.storage_class & STC.deprecated_);
    }

    /// Flag this aggregate as deprecated
    extern (D) final void setDeprecated()
    {
        this.storage_class |= STC.deprecated_;
    }

    /****************************************
     * Returns true if there's an extra member which is the 'this'
     * pointer to the enclosing context (enclosing aggregate or function)
     */
    final bool isNested() const
    {
        return enclosing !is null;
    }

    /* Append vthis field (this.tupleof[$-1]) to make this aggregate type nested.
     */
    extern (D) final void makeNested()
    {
        if (enclosing) // if already nested
            return;
        if (sizeok == Sizeok.done)
            return;
        if (isUnionDeclaration() || isInterfaceDeclaration())
            return;
        if (storage_class & STC.static_)
            return;

        // If nested struct, add in hidden 'this' pointer to outer scope
        auto s = toParentLocal();
        if (!s)
            s = toParent2();
        if (!s)
            return;
        Type t = null;
        if (auto fd = s.isFuncDeclaration())
        {
            enclosing = fd;

            /* https://issues.dlang.org/show_bug.cgi?id=14422
             * If a nested class parent is a function, its
             * context pointer (== `outer`) should be void* always.
             */
            t = Type.tvoidptr;
        }
        else if (auto ad = s.isAggregateDeclaration())
        {
            if (isClassDeclaration() && ad.isClassDeclaration())
            {
                enclosing = ad;
            }
            else if (isStructDeclaration())
            {
                if (auto ti = ad.parent.isTemplateInstance())
                {
                    enclosing = ti.enclosing;
                }
            }
            t = ad.handleType();
        }
        if (enclosing)
        {
            //printf("makeNested %s, enclosing = %s\n", toChars(), enclosing.toChars());
            assert(t);
            if (t.ty == Tstruct)
                t = Type.tvoidptr; // t should not be a ref type

            assert(!vthis);
            vthis = new ThisDeclaration(loc, t);
            //vthis.storage_class |= STC.ref_;

            // Emulate vthis.addMember()
            members.push(vthis);

            // Emulate vthis.dsymbolSemantic()
            vthis.storage_class |= STC.field;
            vthis.parent = this;
            vthis.visibility = Visibility(Visibility.Kind.public_);
            vthis.alignment = t.alignment();
            vthis.semanticRun = PASS.semanticdone;

            if (sizeok == Sizeok.fwd)
                fields.push(vthis);

            makeNested2();
        }
    }

    /* Append vthis2 field (this.tupleof[$-1]) to add a second context pointer.
     */
    extern (D) final void makeNested2()
    {
        if (vthis2)
            return;
        if (!vthis)
            makeNested();   // can't add second before first
        if (!vthis)
            return;
        if (sizeok == Sizeok.done)
            return;
        if (isUnionDeclaration() || isInterfaceDeclaration())
            return;
        if (storage_class & STC.static_)
            return;

        auto s0 = toParentLocal();
        auto s = toParent2();
        if (!s || !s0 || s == s0)
            return;
        auto cd = s.isClassDeclaration();
        Type t = cd ? cd.type : Type.tvoidptr;

        vthis2 = new ThisDeclaration(loc, t);
        //vthis2.storage_class |= STC.ref_;

        // Emulate vthis2.addMember()
        members.push(vthis2);

        // Emulate vthis2.dsymbolSemantic()
        vthis2.storage_class |= STC.field;
        vthis2.parent = this;
        vthis2.visibility = Visibility(Visibility.Kind.public_);
        vthis2.alignment = t.alignment();
        vthis2.semanticRun = PASS.semanticdone;

        if (sizeok == Sizeok.fwd)
            fields.push(vthis2);
    }

    override final bool isExport() const
    {
        return visibility.kind == Visibility.Kind.export_;
    }

    override final Visibility visible() pure nothrow @nogc @safe
    {
        return visibility;
    }

    // 'this' type
    final Type handleType()
    {
        return type;
    }

    // Does this class have an invariant function?
    final bool hasInvariant()
    {
        return invs.length != 0;
    }

    // Back end
    void* sinit;  /// initializer symbol

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/****************************
 * Do byte or word alignment as necessary.
 * Align sizes of 0, as we may not know array sizes yet.
 * Params:
 *   alignment = struct alignment that is in effect
 *   memalignsize = natural alignment of field
 *   offset = offset to be aligned
 * Returns:
 *   aligned offset
 */
public uint alignmember(structalign_t alignment, uint memalignsize, uint offset) pure nothrow @safe
{
    //debug printf("alignment = %u %d, size = %u, offset = %u\n", alignment.get(), alignment.isPack(), memalignsize, offset);
    uint alignvalue;

    if (alignment.isDefault())
    {
        // Alignment in Target::fieldalignsize must match what the
        // corresponding C compiler's default alignment behavior is.
        alignvalue = memalignsize;
    }
    else if (alignment.isPack())    // #pragma pack semantics
    {
        alignvalue = alignment.get();
        if (memalignsize < alignvalue)
            alignvalue = memalignsize;      // align to min(memalignsize, alignment)
    }
    else if (alignment.get() > 1)
    {
        // Align on alignment boundary, which must be a positive power of 2
        alignvalue = alignment.get();
    }
    else
        return offset;

    assert(alignvalue && !(alignvalue & (alignvalue - 1))); // non-zero and power of 2
    return (offset + alignvalue - 1) & ~(alignvalue - 1);
}

/****************************************
 * Place a field (mem) into an aggregate (agg), which can be a struct, union or class
 * Params:
 *    loc           = source location for error messages
 *    nextoffset    = location just past the end of the previous field in the aggregate.
 *                    Updated to be just past the end of this field to be placed, i.e. the future nextoffset
 *    memsize       = size of field
 *    memalignsize  = natural alignment of field
 *    alignment     = alignment in effect for this field
 *    aggsize       = size of aggregate (updated)
 *    aggalignsize  = alignment of aggregate (updated)
 *    isunion       = the aggregate is a union
 * Returns:
 *    aligned offset to place field at
 *
 */
public uint placeField(Loc loc, ref uint nextoffset, uint memsize, uint memalignsize,
    structalign_t alignment, ref uint aggsize, ref uint aggalignsize, bool isunion) @trusted nothrow
{
    static if (0)
    {
        printf("placeField() nextoffset:   %u\n", nextoffset);
        printf(":            memsize:      %u\n", memsize);
        printf(":            memalignsize: %u\n", memalignsize);
        printf(":            alignment:    %u\n", alignment.get());
        printf(":            aggsize:      %u\n", aggsize);
        printf(":            aggalignsize: %u\n", aggalignsize);
        printf(":            isunion:      %d\n", isunion);
    }

    uint ofs = nextoffset;

    const uint actualAlignment =
        alignment.isDefault() || alignment.isPack() && memalignsize < alignment.get()
                    ? memalignsize : alignment.get();

    // Ensure no overflow for (memsize + actualAlignment + ofs)
    bool overflow;
    const sz = addu(memsize, actualAlignment, overflow);
    addu(ofs, sz, overflow);
    if (overflow)
    {
        error(loc, "max object size %u exceeded from adding field size %u + alignment adjustment %u + field offset %u when placing field in aggregate",
                uint.max, memsize, actualAlignment, ofs);
        return 0;
    }

    // Skip no-op for noreturn without custom aligment
    if (memalignsize != 0 || !alignment.isDefault())
        ofs = alignmember(alignment, memalignsize, ofs);

    uint memoffset = ofs;
    ofs += memsize;
    if (ofs > aggsize)
        aggsize = ofs;
    if (!isunion)
    {
        nextoffset = ofs;
        //printf("     revised nextoffset:   %u\n", ofs);
    }

    if (aggalignsize < actualAlignment)
        aggalignsize = actualAlignment;

    return memoffset;
}
