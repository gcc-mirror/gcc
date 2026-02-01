/**
 * Miscellaneous declarations, including typedef, alias, variable declarations including the
 * implicit this declaration, type tuples, ClassInfo, ModuleInfo and various TypeInfos.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/declaration.d, _declaration.d)
 * Documentation:  https://dlang.org/phobos/dmd_declaration.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/declaration.d
 */

module dmd.declaration;

import core.stdc.stdio;
import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.dsymbol;
import dmd.dtemplate;
import dmd.errors;
import dmd.errorsink;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.intrange;
import dmd.location;
import dmd.mtype;
import dmd.common.outbuffer;
import dmd.root.filename;
import dmd.target;
import dmd.targetcompiler;
import dmd.visitor;


/******************************************
 */
void ObjectNotFound(Loc loc, Identifier id)
{
    global.gag = 0; // never gag the fatal error
    const dmdConfFile = global.inifilename.length ? FileName.canonicalName(global.inifilename) : "not found";

    mixin HostObjectNotFound;
    hostObjectNotFound(loc, id.toChars(), dmdConfFile, global.errorSink); // print host-specific diagnostic
    fatal();
}

/* Accumulator for successive matches.
 */
struct MatchAccumulator
{
    int count;              // number of matches found so far
    MATCH last = MATCH.nomatch; // match level of lastf
    FuncDeclaration lastf;  // last matching function we found
    FuncDeclaration nextf;  // if ambiguous match, this is the "other" function
}

/***********************************************************
 */
extern (C++) abstract class Declaration : Dsymbol
{
    Type type;
    Type originalType;  // before semantic analysis
    STC storage_class = STC.none;
    // overridden symbol with pragma(mangle, "...")
    const(char)[] mangleOverride;
    Visibility visibility;
    short inuse;          // used to detect cycles

    private extern (D) static struct BitFields
    {
        LINK _linkage = LINK.default_; // may be `LINK.system`; use `resolvedLinkage()` to resolve it
        bool wasRead;      // set if AliasDeclaration was read
        bool ignoreRead;   // ignore any reads of AliasDeclaration
        bool noUnderscore; // don't prepend _ to mangled name
        bool hidden;       // don't print this in .di files
        bool nrvo;         /// forward to fd.nrvo_var when generating code
    }

    import dmd.common.bitfields;
    mixin(generateBitFields!(BitFields, ubyte));

    final extern (D) this(DSYM tag, Identifier ident) @safe
    {
        super(tag, ident);
        visibility = Visibility(Visibility.Kind.undefined);
    }

    final extern (D) this(DSYM tag, Loc loc, Identifier ident) @safe
    {
        super(tag, loc, ident);
        visibility = Visibility(Visibility.Kind.undefined);
    }

    override const(char)* kind() const
    {
        return "declaration";
    }

    final bool isStatic() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.static_) != 0;
    }

    /// Returns the linkage, resolving the target-specific `System` one.
    final LINK resolvedLinkage() const
    {
        return _linkage == LINK.system ? target.systemLinkage() : _linkage;
    }

    bool isDelete()
    {
        return false;
    }

    bool isDataseg()
    {
        return false;
    }

    bool isThreadlocal()
    {
        return false;
    }

    bool isCodeseg() const pure nothrow @nogc @safe
    {
        return false;
    }

    final bool isFinal() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.final_) != 0;
    }

    bool isAbstract()
    {
        return (storage_class & STC.abstract_) != 0;
    }

    final bool isConst() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.const_) != 0;
    }

    final bool isImmutable() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.immutable_) != 0;
    }

    final bool isWild() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.wild) != 0;
    }

    final bool isAuto() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.auto_) != 0;
    }

    final bool isScope() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.scope_) != 0;
    }

    final bool isReturn() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.return_) != 0;
    }

    final bool isSynchronized() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.synchronized_) != 0;
    }

    final bool isParameter() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.parameter) != 0;
    }

    override final bool isDeprecated() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.deprecated_) != 0;
    }

    final bool isDisabled() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.disable) != 0;
    }

    final bool isOverride() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.override_) != 0;
    }

    final bool isResult() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.result) != 0;
    }

    final bool isField() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.field) != 0;
    }

    final bool isIn() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.in_) != 0;
    }

    final bool isOut() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.out_) != 0;
    }

    final bool isRef() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.ref_) != 0;
    }

    /// Returns: Whether the variable is a reference, annotated with `out` or `ref`
    final bool isReference() const pure nothrow @nogc @safe
    {
        return (storage_class & (STC.ref_ | STC.out_)) != 0;
    }

    final bool isFuture() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.future) != 0;
    }

    final extern(D) bool isSystem() const pure nothrow @nogc @safe
    {
        return (storage_class & STC.system) != 0;
    }

    override final Visibility visible() pure nothrow @nogc @safe
    {
        return visibility;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TupleDeclaration : Declaration
{
    Objects* objects;
    TypeTuple tupletype;    // !=null if this is a type tuple
    bool isexp;             // true: expression tuple
    bool building;          // it's growing in AliasAssign semantic

    extern (D) this(Loc loc, Identifier ident, Objects* objects) @safe
    {
        super(DSYM.tupleDeclaration, loc, ident);
        this.objects = objects;
    }

    override TupleDeclaration syntaxCopy(Dsymbol s)
    {
        assert(0);
    }

    override const(char)* kind() const
    {
        return "sequence";
    }

    override bool needThis()
    {
        //printf("TupleDeclaration::needThis(%s)\n", toChars());
        return isexp ? foreachVar((s) { return s.needThis(); }) != 0 : false;
    }

    /***********************************************************
     * Calls dg(Dsymbol) for each Dsymbol, which should be a VarDeclaration
     * inside VarExp (isexp == true).
     * Params:
     *    dg = delegate to call for each Dsymbol
     */
    extern (D) void foreachVar(scope void delegate(Dsymbol) dg)
    {
        assert(isexp);
        foreach (o; *objects)
        {
            if (auto e = o.isExpression())
                if (auto ve = e.isVarExp())
                    dg(ve.var);
        }
    }

    /***********************************************************
     * Calls dg(Dsymbol) for each Dsymbol, which should be a VarDeclaration
     * inside VarExp (isexp == true).
     * If dg returns !=0, stops and returns that value else returns 0.
     * Params:
     *    dg = delegate to call for each Dsymbol
     * Returns:
     *    last value returned by dg()
     */
    extern (D) int foreachVar(scope int delegate(Dsymbol) dg)
    {
        assert(isexp);
        foreach (o; *objects)
        {
            if (auto e = o.isExpression())
                if (auto ve = e.isVarExp())
                    if(auto ret = dg(ve.var))
                        return ret;
        }
        return 0;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/declaration.html#AliasDeclaration
 */
extern (C++) final class AliasDeclaration : Declaration
{
    Dsymbol aliassym;   // alias ident = aliassym;

    Dsymbol overnext;   // next in overload list
    Dsymbol _import;    // !=null if unresolved internal alias for selective import

    extern (D) this(Loc loc, Identifier ident, Type type) @safe
    {
        super(DSYM.aliasDeclaration, loc, ident);
        //debug printf("AliasDeclaration(id = '%s', type = `%s`, %p)\n", ident.toChars(), dmd.hdrgen.toChars(type), type.isTypeIdentifier());
        this.type = type;
        assert(type);
    }

    extern (D) this(Loc loc, Identifier ident, Dsymbol s) @safe
    {
        super(DSYM.aliasDeclaration, loc, ident);
        //debug printf("AliasDeclaration(id = '%s', s = `%s`)\n", ident.toChars(), s.toChars());
        assert(s != this);
        this.aliassym = s;
        assert(s);
    }

    static AliasDeclaration create(Loc loc, Identifier id, Type type) @safe
    {
        return new AliasDeclaration(loc, id, type);
    }

    override AliasDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("AliasDeclaration::syntaxCopy()\n");
        assert(!s);
        AliasDeclaration sa = type ? new AliasDeclaration(loc, ident, type.syntaxCopy()) : new AliasDeclaration(loc, ident, aliassym.syntaxCopy(null));
        sa.comment = comment;
        sa.storage_class = storage_class;
        return sa;
    }

    override const(char)* kind() const
    {
        return "alias";
    }

    override bool isOverloadable() const
    {
        // assume overloadable until alias is resolved
        return semanticRun < PASS.semanticdone ||
            aliassym && aliassym.isOverloadable();
    }

    /** Returns: `true` if this instance was created to make a template parameter
    visible in the scope of a template body, `false` otherwise */
    extern (D) bool isAliasedTemplateParameter() const
    {
        return !!(storage_class & STC.templateparameter);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class OverDeclaration : Declaration
{
    Dsymbol overnext;   // next in overload list
    Dsymbol aliassym;

    extern (D) this(Identifier ident, Dsymbol s) @safe
    {
        super(DSYM.overDeclaration, ident);
        this.aliassym = s;
    }

    override const(char)* kind() const
    {
        return "overload alias"; // todo
    }

    override bool isOverloadable() const
    {
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) class VarDeclaration : Declaration
{
    Initializer _init;
    FuncDeclarations nestedrefs;    // referenced by these lexically nested functions
    TupleDeclaration aliasTuple;    // when `this` is really a tuple of declarations
    VarDeclaration lastVar;         // Linked list of variables for goto-skips-init detection
    Expression edtor;               // if !=null, does the destruction of the variable
    IntRange* range;                // if !=null, the variable is known to be within the range

    uint endlinnum;                 // line number of end of scope that this var lives in
    uint offset;
    uint sequenceNumber;            // order the variables are declared
    structalign_t alignment;

    // When interpreting, these point to the value (NULL if value not determinable)
    // The index of this variable on the CTFE stack, AdrOnStackNone if not allocated
    enum AdrOnStackNone = ~0u;
    uint ctfeAdrOnStack;

    // `bool` fields that are compacted into bit fields in a string mixin
    private extern (D) static struct BitFields
    {
        bool isargptr;          /// if parameter that _argptr points to
        bool ctorinit;          /// it has been initialized in a ctor
        bool iscatchvar;        /// this is the exception object variable in catch() clause
        bool isowner;           /// this is an Owner, despite it being `scope`
        bool setInCtorOnly;     /// field can only be set in a constructor, as it is const or immutable

        /// It is a class that was allocated on the stack
        ///
        /// This means the var is not rebindable once assigned,
        /// and the destructor gets run when it goes out of scope
        bool onstack;

        bool overlapped;        /// if it is a field and has overlapping
        bool overlapUnsafe;     /// if it is an overlapping field and the overlaps are unsafe
        bool maybeScope;        /// allow inferring 'scope' for this variable
        bool doNotInferReturn;  /// do not infer 'return' for this variable

        bool isArgDtorVar;      /// temporary created to handle scope destruction of a function argument
        bool isCmacro;          /// it is a C macro turned into a C declaration
        bool dllImport;         /// __declspec(dllimport)
        bool dllExport;         /// __declspec(dllexport)
        mixin VarDeclarationExtra;
        bool systemInferred;    /// @system was inferred from initializer
    }

    import dmd.common.bitfields : generateBitFields;
    mixin(generateBitFields!(BitFields, uint));

    byte canassign;                 // it can be assigned to
    ubyte isdataseg;                // private data for isDataseg 0 unset, 1 true, 2 false

    final extern (D) this(Loc loc, Type type, Identifier ident, Initializer _init, STC storage_class = STC.none)
    in
    {
        assert(ident);
    }
    do
    {
        //printf("VarDeclaration('%s')\n", ident.toChars());
        super(DSYM.varDeclaration, loc, ident);
        debug
        {
            if (!type && !_init)
            {
                //printf("VarDeclaration('%s')\n", ident.toChars());
                //*(char*)0=0;
            }
        }

        assert(type || _init);
        this.type = type;
        this._init = _init;
        ctfeAdrOnStack = AdrOnStackNone;
        this.storage_class = storage_class;
    }

    static VarDeclaration create(Loc loc, Type type, Identifier ident, Initializer _init, StorageClass storage_class = STC.none)
    {
        return new VarDeclaration(loc, type, ident, _init, cast(STC) storage_class);
    }

    override VarDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("VarDeclaration::syntaxCopy(%s)\n", toChars());
        assert(!s);
        auto v = new VarDeclaration(loc, type ? type.syntaxCopy() : null, ident, _init ? _init.syntaxCopy() : null, storage_class);
        v.comment = comment;
        return v;
    }

    override const(char)* kind() const
    {
        return "variable";
    }

    override final inout(AggregateDeclaration) isThis() inout
    {
        if (storage_class & (STC.static_ | STC.extern_ | STC.manifest | STC.templateparameter | STC.gshared | STC.ctfe))
            return null;

        /* The casting is necessary because `s = s.parent` is otherwise rejected
         */
        for (auto s = cast(Dsymbol)this; s; s = s.parent)
        {
            if (auto ad = (cast(inout)s).isMember())
                return ad;
            if (!s.parent || !s.parent.isTemplateMixin())
                break;
        }

        return null;
    }

    override final bool needThis()
    {
        //printf("VarDeclaration::needThis(%s, x%x)\n", toChars(), storage_class);
        return isField();
    }

    override final bool isExport() const
    {
        return visibility.kind == Visibility.Kind.export_ || dllExport;
    }

    override final bool isImportedSymbol() const
    {
        /* If global variable has `export` and `extern` then it is imported
         *   export int sym1;            // definition:  exported
         *   export extern int sym2;     // declaration: imported
         *   export extern int sym3 = 0; // error, extern cannot have initializer
         */
        bool result =
            dllImport ||
            visibility.kind == Visibility.Kind.export_ &&
            storage_class & STC.extern_ &&
            (storage_class & STC.static_ || parent.isModule());
        //printf("isImportedSymbol() %s %d\n", toChars(), result);
        return result;
    }

    final bool isCtorinit() const pure nothrow @nogc @safe
    {
        return setInCtorOnly;
    }

    /*******************************
     * Does symbol go into data segment?
     * Includes extern variables.
     */
    override final bool isDataseg()
    {
        version (none)
        {
            printf("VarDeclaration::isDataseg(%p, '%s')\n", this, toChars());
            printf("%llx, isModule: %p, isTemplateInstance: %p, isNspace: %p\n",
                   storage_class & (STC.static_ | STC.const_), parent.isModule(), parent.isTemplateInstance(), parent.isNspace());
            printf("parent = '%s'\n", parent.toChars());
        }

        if (isdataseg == 0) // the value is not cached
        {
            isdataseg = 2; // The Variables does not go into the datasegment

            if (!canTakeAddressOf() || (storage_class & STC.exptemp))
            {
                return false;
            }

            Dsymbol parent = toParent();
            if (!parent && !(storage_class & STC.static_))
            {
                .error(loc, "%s `%s` forward referenced", kind, toPrettyChars);
                type = Type.terror;
            }
            else if (storage_class & (STC.static_ | STC.extern_ | STC.gshared) ||
                parent.isModule() || parent.isTemplateInstance() || parent.isNspace())
            {
                assert(!isParameter() && !isResult());
                isdataseg = 1; // It is in the DataSegment
            }
        }

        return (isdataseg == 1);
    }
    /************************************
     * Does symbol go into thread local storage?
     */
    override final bool isThreadlocal()
    {
        //printf("VarDeclaration::isThreadlocal(%p, '%s')\n", this, toChars());
        /* Data defaults to being thread-local. It is not thread-local
         * if it is immutable, const or shared.
         */
        bool i = isDataseg() && !(storage_class & (STC.immutable_ | STC.const_ | STC.shared_ | STC.gshared));
        //printf("\treturn %d\n", i);
        return i;
    }

    /********************************************
     * Can variable be read and written by CTFE?
     */
    final bool isCTFE()
    {
        return (storage_class & STC.ctfe) != 0; // || !isDataseg();
    }

    /*************************************
     * Return true if we can take the address of this variable.
     */
    final bool canTakeAddressOf() @safe
    {
        return !(storage_class & STC.manifest);
    }

    /******************************************
     * Return true if variable needs to call the destructor.
     */
    final bool needsScopeDtor() @safe
    {
        //printf("VarDeclaration::needsScopeDtor() %s %d\n", toChars(), edtor && !(storage_class & STC.nodtor));
        return edtor && !(storage_class & STC.nodtor);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/*******************************************************
 * C11 6.7.2.1-4 bit fields
 */
extern (C++) class BitFieldDeclaration : VarDeclaration
{
    Expression width;

    uint fieldWidth;
    uint bitOffset;

    final extern (D) this(Loc loc, Type type, Identifier ident, Expression width)
    {
        super(loc, type, ident, null);
        this.dsym = DSYM.bitFieldDeclaration;
        this.width = width;
        this.storage_class |= STC.field;
    }

    override BitFieldDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("BitFieldDeclaration::syntaxCopy(%s)\n", toChars());
        assert(!s);
        auto bf = new BitFieldDeclaration(loc, type ? type.syntaxCopy() : null, ident, width.syntaxCopy());
        bf.comment = comment;
        return bf;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * This is a shell around a back end symbol
 */
extern (C++) final class SymbolDeclaration : Declaration
{
    AggregateDeclaration dsym;

    extern (D) this(Loc loc, AggregateDeclaration dsym) @safe
    {
        super(DSYM.symbolDeclaration, loc, dsym.ident);
        this.dsym = dsym;
        storage_class |= STC.const_;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
private Identifier getTypeInfoIdent(Type t)
{
    import dmd.mangle;
    import core.stdc.stdlib;
    import dmd.root.rmem;
    // _init_10TypeInfo_%s
    OutBuffer buf;
    buf.reserve(32);
    mangleToBuffer(t, buf);

    const slice = buf[];

    // Allocate buffer on stack, fail over to using malloc()
    char[128] namebuf;
    const namelen = 19 + size_t.sizeof * 3 + slice.length + 1;
    auto name = namelen <= namebuf.length ? namebuf.ptr : cast(char*)Mem.check(malloc(namelen));

    const length = snprintf(name, namelen, "_D%lluTypeInfo_%.*s6__initZ",
            cast(ulong)(9 + slice.length), cast(int)slice.length, slice.ptr);
    //printf("%p %s, deco = %s, name = %s\n", this, toChars(), deco, name);
    assert(0 < length && length < namelen); // don't overflow the buffer

    auto id = Identifier.idPool(name[0 .. length]);

    if (name != namebuf.ptr)
        free(name);
    return id;
}

extern (C++) class TypeInfoDeclaration : VarDeclaration
{
    Type tinfo;

    final extern (D) this(Type tinfo)
    {
        super(Loc.initial, Type.dtypeinfo.type, tinfo.getTypeInfoIdent(), null);
        this.dsym = DSYM.typeInfoDeclaration;
        this.tinfo = tinfo;
        storage_class = STC.static_ | STC.gshared;
        visibility = Visibility(Visibility.Kind.public_);
        _linkage = LINK.c;
        alignment.set(target.ptrsize);
    }

    static TypeInfoDeclaration create(Type tinfo)
    {
        return new TypeInfoDeclaration(tinfo);
    }

    override final TypeInfoDeclaration syntaxCopy(Dsymbol s)
    {
        assert(0); // should never be produced by syntax
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoStructDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfostruct)
        {
            ObjectNotFound(loc, Id.TypeInfo_Struct);
        }
        type = Type.typeinfostruct.type;
    }

    static TypeInfoStructDeclaration create(Type tinfo)
    {
        return new TypeInfoStructDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoClassDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoclass)
        {
            ObjectNotFound(loc, Id.TypeInfo_Class);
        }
        type = Type.typeinfoclass.type;
    }

    static TypeInfoClassDeclaration create(Type tinfo)
    {
        return new TypeInfoClassDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoInterfaceDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfointerface)
        {
            ObjectNotFound(loc, Id.TypeInfo_Interface);
        }
        type = Type.typeinfointerface.type;
    }

    static TypeInfoInterfaceDeclaration create(Type tinfo)
    {
        return new TypeInfoInterfaceDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoPointerDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfopointer)
        {
            ObjectNotFound(loc, Id.TypeInfo_Pointer);
        }
        type = Type.typeinfopointer.type;
    }

    static TypeInfoPointerDeclaration create(Type tinfo)
    {
        return new TypeInfoPointerDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoArrayDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoarray)
        {
            ObjectNotFound(loc, Id.TypeInfo_Array);
        }
        type = Type.typeinfoarray.type;
    }

    static TypeInfoArrayDeclaration create(Type tinfo)
    {
        return new TypeInfoArrayDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoStaticArrayDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfostaticarray)
        {
            ObjectNotFound(loc, Id.TypeInfo_StaticArray);
        }
        type = Type.typeinfostaticarray.type;
    }

    static TypeInfoStaticArrayDeclaration create(Type tinfo)
    {
        return new TypeInfoStaticArrayDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoAssociativeArrayDeclaration : TypeInfoDeclaration
{
    Type entry; // type of TypeInfo_AssociativeArray.Entry!(t.index, t.next)
    Declaration xopEqual; // implementation of TypeInfo_AssociativeArray.equals
    Declaration xtoHash;  // implementation of TypeInfo_AssociativeArray.getHash

    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoassociativearray)
        {
            ObjectNotFound(loc, Id.TypeInfo_AssociativeArray);
        }
        type = Type.typeinfoassociativearray.type;
    }

    static TypeInfoAssociativeArrayDeclaration create(Type tinfo)
    {
        return new TypeInfoAssociativeArrayDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoEnumDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoenum)
        {
            ObjectNotFound(loc, Id.TypeInfo_Enum);
        }
        type = Type.typeinfoenum.type;
    }

    static TypeInfoEnumDeclaration create(Type tinfo)
    {
        return new TypeInfoEnumDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoFunctionDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfofunction)
        {
            ObjectNotFound(loc, Id.TypeInfo_Function);
        }
        type = Type.typeinfofunction.type;
    }

    static TypeInfoFunctionDeclaration create(Type tinfo)
    {
        return new TypeInfoFunctionDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoDelegateDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfodelegate)
        {
            ObjectNotFound(loc, Id.TypeInfo_Delegate);
        }
        type = Type.typeinfodelegate.type;
    }

    static TypeInfoDelegateDeclaration create(Type tinfo)
    {
        return new TypeInfoDelegateDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoTupleDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfotypelist)
        {
            ObjectNotFound(loc, Id.TypeInfo_Tuple);
        }
        type = Type.typeinfotypelist.type;
    }

    static TypeInfoTupleDeclaration create(Type tinfo)
    {
        return new TypeInfoTupleDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoConstDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoconst)
        {
            ObjectNotFound(loc, Id.TypeInfo_Const);
        }
        type = Type.typeinfoconst.type;
    }

    static TypeInfoConstDeclaration create(Type tinfo)
    {
        return new TypeInfoConstDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoInvariantDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoinvariant)
        {
            ObjectNotFound(loc, Id.TypeInfo_Invariant);
        }
        type = Type.typeinfoinvariant.type;
    }

    static TypeInfoInvariantDeclaration create(Type tinfo)
    {
        return new TypeInfoInvariantDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoSharedDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfoshared)
        {
            ObjectNotFound(loc, Id.TypeInfo_Shared);
        }
        type = Type.typeinfoshared.type;
    }

    static TypeInfoSharedDeclaration create(Type tinfo)
    {
        return new TypeInfoSharedDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoWildDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfowild)
        {
            ObjectNotFound(loc, Id.TypeInfo_Wild);
        }
        type = Type.typeinfowild.type;
    }

    static TypeInfoWildDeclaration create(Type tinfo)
    {
        return new TypeInfoWildDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class TypeInfoVectorDeclaration : TypeInfoDeclaration
{
    extern (D) this(Type tinfo)
    {
        super(tinfo);
        if (!Type.typeinfovector)
        {
            ObjectNotFound(loc, Id.TypeInfo_Vector);
        }
        type = Type.typeinfovector.type;
    }

    static TypeInfoVectorDeclaration create(Type tinfo)
    {
        return new TypeInfoVectorDeclaration(tinfo);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * For the "this" parameter to member functions
 */
extern (C++) final class ThisDeclaration : VarDeclaration
{
    extern (D) this(Loc loc, Type t)
    {
        super(loc, t, Id.This, null);
        this.dsym = DSYM.thisDeclaration;
        storage_class |= STC.nodtor;
    }

    override ThisDeclaration syntaxCopy(Dsymbol s)
    {
        assert(0); // should never be produced by syntax
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}
