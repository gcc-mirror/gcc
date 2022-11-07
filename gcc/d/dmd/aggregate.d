/**
 * Defines a `Dsymbol` representing an aggregate, which is a `struct`, `union` or `class`.
 *
 * Specification: $(LINK2 https://dlang.org/spec/struct.html, Structs, Unions),
 *                $(LINK2 https://dlang.org/spec/class.html, Class).
 *
 * Copyright:   Copyright (C) 1999-2022 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/aggregate.d, _aggregate.d)
 * Documentation:  https://dlang.org/phobos/dmd_aggregate.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/aggregate.d
 */

module dmd.aggregate;

import core.stdc.stdio;
import core.checkedint;

import dmd.aliasthis;
import dmd.apply;
import dmd.arraytypes;
import dmd.astenums;
import dmd.attrib;
import dmd.declaration;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.mtype;
import dmd.tokens;
import dmd.typesem : defaultInit;
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
const(char)* toChars(ClassKind c)
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
    StorageClass storage_class; ///
    uint structsize;            /// size of struct
    uint alignsize;             /// size of struct for alignment purposes
    VarDeclarations fields;     /// VarDeclaration fields
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

    final extern (D) this(const ref Loc loc, Identifier id)
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
        sc2.explicitVisibility = 0;
        sc2.aligndecl = null;
        sc2.userAttribDecl = null;
        sc2.namespace = null;
        return sc2;
    }

    override final void setScope(Scope* sc)
    {
        // Might need a scope to resolve forward references. The check for
        // semanticRun prevents unnecessary setting of _scope during deferred
        // setScope phases for aggregates which already finished semantic().
        // See https://issues.dlang.org/show_bug.cgi?id=16607
        if (semanticRun < PASS.semanticdone)
            ScopeDsymbol.setScope(sc);
    }

    /***************************************
     * Returns:
     *      The total number of fields minus the number of hidden fields.
     */
    final size_t nonHiddenFields()
    {
        return fields.dim - isNested() - (vthis2 !is null);
    }

    /***************************************
     * Collect all instance fields, then determine instance size.
     * Returns:
     *      false if failed to determine the size.
     */
    final bool determineSize(const ref Loc loc)
    {
        //printf("AggregateDeclaration::determineSize() %s, sizeok = %d\n", toChars(), sizeok);

        // The previous instance size finalizing had:
        if (type.ty == Terror)
            return false;   // failed already
        if (sizeok == Sizeok.done)
            return true;    // succeeded

        if (!members)
        {
            error(loc, "unknown size");
            return false;
        }

        if (_scope)
            dsymbolSemantic(this, null);

        // Determine the instance size of base class first.
        if (auto cd = isClassDeclaration())
        {
            cd = cd.baseClass;
            if (cd && !cd.determineSize(loc))
                goto Lfail;
        }

        // Determine instance fields when sizeok == Sizeok.none
        if (!this.determineFields())
            goto Lfail;
        if (sizeok != Sizeok.done)
            finalizeSize();

        // this aggregate type has:
        if (type.ty == Terror)
            return false;   // marked as invalid during the finalizing.
        if (sizeok == Sizeok.done)
            return true;    // succeeded to calculate instance size.

    Lfail:
        // There's unresolvable forward reference.
        if (type != Type.terror)
            error(loc, "no size because of forward reference");
        // Don't cache errors from speculative semantic, might be resolvable later.
        // https://issues.dlang.org/show_bug.cgi?id=16574
        if (!global.gag)
        {
            type = Type.terror;
            errors = true;
        }
        return false;
    }

    abstract void finalizeSize();

    override final uinteger_t size(const ref Loc loc)
    {
        //printf("+AggregateDeclaration::size() %s, scope = %p, sizeok = %d\n", toChars(), _scope, sizeok);
        bool ok = determineSize(loc);
        //printf("-AggregateDeclaration::size() %s, scope = %p, sizeok = %d\n", toChars(), _scope, sizeok);
        return ok ? structsize : SIZE_INVALID;
    }

    /***************************************
     * Calculate field[i].overlapped and overlapUnsafe, and check that all of explicit
     * field initializers have unique memory space on instance.
     * Returns:
     *      true if any errors happen.
     */
    extern (D) final bool checkOverlappedFields()
    {
        //printf("AggregateDeclaration::checkOverlappedFields() %s\n", toChars());
        assert(sizeok == Sizeok.done);
        size_t nfields = fields.dim;
        if (isNested())
        {
            auto cd = isClassDeclaration();
            if (!cd || !cd.baseClass || !cd.baseClass.isNested())
                nfields--;
            if (vthis2 && !(cd && cd.baseClass && cd.baseClass.vthis2))
                nfields--;
        }
        bool errors = false;

        // Fill in missing any elements with default initializers
        foreach (i; 0 .. nfields)
        {
            auto vd = fields[i];
            if (vd.errors)
            {
                errors = true;
                continue;
            }

            const vdIsVoidInit = vd._init && vd._init.isVoidInitializer();

            // Find overlapped fields with the hole [vd.offset .. vd.offset.size()].
            foreach (j; 0 .. nfields)
            {
                if (i == j)
                    continue;
                auto v2 = fields[j];
                if (v2.errors)
                {
                    errors = true;
                    continue;
                }
                if (!vd.isOverlappedWith(v2))
                    continue;

                // vd and v2 are overlapping.
                vd.overlapped = true;
                v2.overlapped = true;

                if (!MODimplicitConv(vd.type.mod, v2.type.mod))
                    v2.overlapUnsafe = true;
                if (!MODimplicitConv(v2.type.mod, vd.type.mod))
                    vd.overlapUnsafe = true;

                if (i > j)
                    continue;

                if (!v2._init)
                    continue;

                if (v2._init.isVoidInitializer())
                    continue;

                if (vd._init && !vdIsVoidInit && v2._init)
                {
                    .error(loc, "overlapping default initialization for field `%s` and `%s`", v2.toChars(), vd.toChars());
                    errors = true;
                }
                else if (v2._init && i < j)
                {
                    .error(v2.loc, "union field `%s` with default initialization `%s` must be before field `%s`",
                        v2.toChars(), v2._init.toChars(), vd.toChars());
                    errors = true;
                }
            }
        }
        return errors;
    }

    /***************************************
     * Fill out remainder of elements[] with default initializers for fields[].
     * Params:
     *      loc         = location
     *      elements    = explicit arguments which given to construct object.
     *      ctorinit    = true if the elements will be used for default initialization.
     * Returns:
     *      false if any errors occur.
     *      Otherwise, returns true and the missing arguments will be pushed in elements[].
     */
    final bool fill(const ref Loc loc, ref Expressions elements, bool ctorinit)
    {
        //printf("AggregateDeclaration::fill() %s\n", toChars());
        assert(sizeok == Sizeok.done);
        const nfields = nonHiddenFields();
        bool errors = false;

        size_t dim = elements.dim;
        elements.setDim(nfields);
        foreach (size_t i; dim .. nfields)
            elements[i] = null;

        // Fill in missing any elements with default initializers
        foreach (i; 0 .. nfields)
        {
            if (elements[i])
                continue;

            auto vd = fields[i];
            auto vx = vd;
            if (vd._init && vd._init.isVoidInitializer())
                vx = null;

            // Find overlapped fields with the hole [vd.offset .. vd.offset.size()].
            size_t fieldi = i;
            foreach (j; 0 .. nfields)
            {
                if (i == j)
                    continue;
                auto v2 = fields[j];
                if (!vd.isOverlappedWith(v2))
                    continue;

                if (elements[j])
                {
                    vx = null;
                    break;
                }
                if (v2._init && v2._init.isVoidInitializer())
                    continue;

                version (all)
                {
                    /* Prefer first found non-void-initialized field
                     * union U { int a; int b = 2; }
                     * U u;    // Error: overlapping initialization for field a and b
                     */
                    if (!vx)
                    {
                        vx = v2;
                        fieldi = j;
                    }
                    else if (v2._init)
                    {
                        .error(loc, "overlapping initialization for field `%s` and `%s`", v2.toChars(), vd.toChars());
                        errors = true;
                    }
                }
                else
                {
                    // fixes https://issues.dlang.org/show_bug.cgi?id=1432 by enabling this path always

                    /* Prefer explicitly initialized field
                     * union U { int a; int b = 2; }
                     * U u;    // OK (u.b == 2)
                     */
                    if (!vx || !vx._init && v2._init)
                    {
                        vx = v2;
                        fieldi = j;
                    }
                    else if (vx != vd && !vx.isOverlappedWith(v2))
                    {
                        // Both vx and v2 fills vd, but vx and v2 does not overlap
                    }
                    else if (vx._init && v2._init)
                    {
                        .error(loc, "overlapping default initialization for field `%s` and `%s`",
                            v2.toChars(), vd.toChars());
                        errors = true;
                    }
                    else
                        assert(vx._init || !vx._init && !v2._init);
                }
            }
            if (vx)
            {
                Expression e;
                if (vx.type.size() == 0)
                {
                    e = null;
                }
                else if (vx._init)
                {
                    assert(!vx._init.isVoidInitializer());
                    if (vx.inuse)   // https://issues.dlang.org/show_bug.cgi?id=18057
                    {
                        vx.error(loc, "recursive initialization of field");
                        errors = true;
                    }
                    else
                        e = vx.getConstInitializer(false);
                }
                else
                {
                    if ((vx.storage_class & STC.nodefaultctor) && !ctorinit)
                    {
                        .error(loc, "field `%s.%s` must be initialized because it has no default constructor",
                            type.toChars(), vx.toChars());
                        errors = true;
                    }
                    /* https://issues.dlang.org/show_bug.cgi?id=12509
                     * Get the element of static array type.
                     */
                    Type telem = vx.type;
                    if (telem.ty == Tsarray)
                    {
                        /* We cannot use Type::baseElemOf() here.
                         * If the bottom of the Tsarray is an enum type, baseElemOf()
                         * will return the base of the enum, and its default initializer
                         * would be different from the enum's.
                         */
                        TypeSArray tsa;
                        while ((tsa = telem.toBasetype().isTypeSArray()) !is null)
                            telem = tsa.next;
                        if (telem.ty == Tvoid)
                            telem = Type.tuns8.addMod(telem.mod);
                    }
                    if (telem.needsNested() && ctorinit)
                        e = telem.defaultInit(loc);
                    else
                        e = telem.defaultInitLiteral(loc);
                }
                elements[fieldi] = e;
            }
        }
        foreach (e; elements)
        {
            if (e && e.op == EXP.error)
                return false;
        }

        return !errors;
    }

    /****************************
     * Do byte or word alignment as necessary.
     * Align sizes of 0, as we may not know array sizes yet.
     * Params:
     *   alignment = struct alignment that is in effect
     *   memalignsize = natural alignment of field
     *   poffset = pointer to offset to be aligned
     */
    extern (D) static void alignmember(structalign_t alignment, uint memalignsize, uint* poffset) pure nothrow @safe
    {
        //debug printf("alignment = %u %d, size = %u, offset = %u\n", alignment.get(), alignment.isPack(), memalignsize, *poffset);
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
            return;

        assert(alignvalue > 0 && !(alignvalue & (alignvalue - 1)));
        *poffset = (*poffset + alignvalue - 1) & ~(alignvalue - 1);
    }

    /****************************************
     * Place a field (mem) into an aggregate (agg), which can be a struct, union or class
     * Params:
     *    nextoffset    = location just past the end of the previous field in the aggregate.
     *                    Updated to be just past the end of this field to be placed, i.e. the future nextoffset
     *    memsize       = size of field
     *    memalignsize  = natural alignment of field
     *    alignment     = alignment in effect for this field
     *    paggsize      = size of aggregate (updated)
     *    paggalignsize = alignment of aggregate (updated)
     *    isunion       = the aggregate is a union
     * Returns:
     *    aligned offset to place field at
     *
     */
    extern (D) static uint placeField(uint* nextoffset, uint memsize, uint memalignsize,
        structalign_t alignment, uint* paggsize, uint* paggalignsize, bool isunion)
    {
        uint ofs = *nextoffset;

        const uint actualAlignment =
            alignment.isDefault() || alignment.isPack() && memalignsize < alignment.get()
                        ? memalignsize : alignment.get();

        // Ensure no overflow
        bool overflow;
        const sz = addu(memsize, actualAlignment, overflow);
        addu(ofs, sz, overflow);
        if (overflow) assert(0);

        // Skip no-op for noreturn without custom aligment
        if (memalignsize != 0 || !alignment.isDefault())
            alignmember(alignment, memalignsize, &ofs);

        uint memoffset = ofs;
        ofs += memsize;
        if (ofs > *paggsize)
            *paggsize = ofs;
        if (!isunion)
            *nextoffset = ofs;

        if (*paggalignsize < actualAlignment)
            *paggalignsize = actualAlignment;

        return memoffset;
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
    final void setDeprecated()
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

    /*******************************************
     * Look for constructor declaration.
     */
    final Dsymbol searchCtor()
    {
        auto s = search(Loc.initial, Id.ctor);
        if (s)
        {
            if (!(s.isCtorDeclaration() ||
                  s.isTemplateDeclaration() ||
                  s.isOverloadSet()))
            {
                s.error("is not a constructor; identifiers starting with `__` are reserved for the implementation");
                errors = true;
                s = null;
            }
        }
        if (s && s.toParent() != this)
            s = null; // search() looks through ancestor classes
        if (s)
        {
            // Finish all constructors semantics to determine this.noDefaultCtor.
            struct SearchCtor
            {
                extern (C++) static int fp(Dsymbol s, void* ctxt)
                {
                    auto f = s.isCtorDeclaration();
                    if (f && f.semanticRun == PASS.initial)
                        f.dsymbolSemantic(null);
                    return 0;
                }
            }

            for (size_t i = 0; i < members.dim; i++)
            {
                auto sm = (*members)[i];
                sm.apply(&SearchCtor.fp, null);
            }
        }
        return s;
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

    override final inout(AggregateDeclaration) isAggregateDeclaration() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}
