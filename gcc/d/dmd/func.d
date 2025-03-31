/**
 * Defines a function declaration.
 *
 * Includes:
 * - function/delegate literals
 * - function aliases
 * - (static/shared) constructors/destructors/post-blits
 * - `invariant`
 * - `unittest`
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/func.d, _func.d)
 * Documentation:  https://dlang.org/phobos/dmd_func.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/func.d
 */

module dmd.func;

import core.stdc.stdio;
import core.stdc.string;
import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.blockexit;
import dmd.gluelayer;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.delegatize;
import dmd.dmodule;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dtemplate;
import dmd.escape;
import dmd.expression;
import dmd.funcsem : overloadApply;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.location;
import dmd.mtype;
import dmd.objc;
import dmd.common.outbuffer;
import dmd.rootobject;
import dmd.root.string;
import dmd.root.stringtable;
import dmd.statement;
import dmd.tokens;
import dmd.visitor;

version (IN_GCC) {}
else version (IN_LLVM) {}
else version = MARS;

/// Inline Status
enum ILS : ubyte
{
    uninitialized,       /// not computed yet
    no,                  /// cannot inline
    yes,                 /// can inline
}

enum BUILTIN : ubyte
{
    unknown = 255,   /// not known if this is a builtin
    unimp = 0,       /// this is not a builtin
    gcc,             /// this is a GCC builtin
    llvm,            /// this is an LLVM builtin
    sin,
    cos,
    tan,
    sqrt,
    fabs,
    ldexp,
    log,
    log2,
    log10,
    exp,
    expm1,
    exp2,
    round,
    floor,
    ceil,
    trunc,
    copysign,
    pow,
    fmin,
    fmax,
    fma,
    isnan,
    isinfinity,
    isfinite,
    bsf,
    bsr,
    bswap,
    popcnt,
    yl2x,
    yl2xp1,
    toPrecFloat,
    toPrecDouble,
    toPrecReal,
    ctfeWrite,
}

private struct FUNCFLAG
{
    bool purityInprocess;    /// working on determining purity
    bool safetyInprocess;    /// working on determining safety
    bool nothrowInprocess;   /// working on determining nothrow
    bool nogcInprocess;      /// working on determining @nogc
    bool saferD;             /// do -preview=safer checks if this function has default safety
    bool scopeInprocess;     /// infer `return` and `scope` for parameters
    bool inlineScanned;      /// function has been scanned for inline possibilities
    bool hasCatches;         /// function has try-catch statements
    bool skipCodegen;        /// do not generate code for this function.
    bool printf;             /// is a printf-like function

    bool scanf;              /// is a scanf-like function
    bool noreturn;           /// the function does not return
    bool isNRVO = true;      /// Support for named return value optimization
    bool isNaked;            /// The function is 'naked' (see inline ASM)
    bool isGenerated;        /// The function is compiler generated (e.g. `opCmp`)
    bool isIntroducing;      /// If this function introduces the overload set
    bool hasSemantic3Errors; /// If errors in semantic3 this function's frame ptr
    bool hasNoEH;            /// No exception unwinding is needed
    bool inferRetType;       /// Return type is to be inferred
    bool hasDualContext;     /// has a dual-context 'this' parameter

    bool hasAlwaysInlines;   /// Contains references to functions that must be inlined
    bool isCrtCtor;          /// Has attribute pragma(crt_constructor)
    bool isCrtDtor;          /// Has attribute pragma(crt_destructor)
    bool hasEscapingSiblings;/// Has sibling functions that escape
    bool computedEscapingSiblings; /// `hasEscapingSiblings` has been computed
    bool dllImport;          /// __declspec(dllimport)
    bool dllExport;          /// __declspec(dllexport)

    bool hasReturnExp;         /// Has return exp; statement
    bool hasInlineAsm;         /// Has asm{} statement
    bool hasMultipleReturnExp; /// Has multiple return exp; statements
}

/***********************************************************
 * Tuple of result identifier (possibly null) and statement.
 * This is used to store out contracts: out(id){ ensure }
 */
extern (C++) struct Ensure
{
    Identifier id;
    Statement ensure;

    Ensure syntaxCopy()
    {
        return Ensure(id, ensure.syntaxCopy());
    }

    /*****************************************
     * Do syntax copy of an array of Ensure's.
     */
    static Ensures* arraySyntaxCopy(Ensures* a)
    {
        if (!a)
            return null;

        Ensures* b = a.copy();
        foreach (i, e; *a)
            (*b)[i] = e.syntaxCopy();
        return b;
    }

}

/***********************************************************
 * Most functions don't have contracts, so save memory by grouping
 * this information into a separate struct
 */
private struct ContractInfo
{
    Statements* frequires;              /// in contracts
    Ensures* fensures;                  /// out contracts
    Statement frequire;                 /// lowered in contract
    Statement fensure;                  /// lowered out contract
    FuncDeclaration fdrequire;          /// function that does the in contract
    FuncDeclaration fdensure;           /// function that does the out contract
    Expressions* fdrequireParams;       /// argument list for __require
    Expressions* fdensureParams;        /// argument list for __ensure
}

/***********************************************************
 */
extern (C++) class FuncDeclaration : Declaration
{
    Statement fbody;                    /// function body

    FuncDeclarations foverrides;        /// functions this function overrides

    private ContractInfo* contracts;    /// contract information

    const(char)* mangleString;          /// mangled symbol created from mangleExact()

    VarDeclaration vresult;             /// result variable for out contracts
    LabelDsymbol returnLabel;           /// where the return goes

    bool[size_t] isTypeIsolatedCache;   /// cache for the potentially very expensive isTypeIsolated check

    // used to prevent symbols in different
    // scopes from having the same name
    DsymbolTable localsymtab;
    VarDeclaration vthis;               /// 'this' parameter (member and nested)
    VarDeclaration v_arguments;         /// '_arguments' parameter

    VarDeclaration v_argptr;            /// '_argptr' variable
    VarDeclarations* parameters;        /// Array of VarDeclaration's for parameters
    DsymbolTable labtab;                /// statement label symbol table
    Dsymbol overnext;                   /// next in overload list
    FuncDeclaration overnext0;          /// next in overload list (only used during IFTI)
    Loc endloc;                         /// location of closing curly bracket
    int vtblIndex = -1;                 /// for member functions, index into vtbl[]

    ILS inlineStatusStmt = ILS.uninitialized;
    ILS inlineStatusExp = ILS.uninitialized;
    PINLINE inlining = PINLINE.default_;

    int inlineNest;                     /// !=0 if nested inline

    ForeachStatement fes;               /// if foreach body, this is the foreach
    BaseClass* interfaceVirtual;        /// if virtual, but only appears in base interface vtbl[]
    /** if !=NULL, then this is the type
    of the 'introducing' function
    this one is overriding
    */
    Type tintro;

    STC storage_class2;        /// storage class for template onemember's

    // Things that should really go into Scope

    VarDeclaration nrvo_var;            /// variable to replace with shidden
    Symbol* shidden;                    /// hidden pointer passed to function

    ReturnStatements* returns;

    GotoStatements* gotos;              /// Gotos with forward references

    version (MARS)
    {
        VarDeclarations* alignSectionVars;  /// local variables with alignment needs larger than stackAlign
        Symbol* salignSection;              /// pointer to aligned section, if any
    }

    /// set if this is a known, builtin function we can evaluate at compile time
    BUILTIN builtin = BUILTIN.unknown;

    /// set if someone took the address of this function
    int tookAddressOf;

    bool requiresClosure;               // this function needs a closure

    /** local variables in this function which are referenced by nested functions
     * (They'll get put into the "closure" for this function.)
     */
    VarDeclarations closureVars;

    /** Outer variables which are referenced by this nested function
     * (the inverse of closureVars)
     */
    VarDeclarations outerVars;

    // Most recent encountered `main` (`WinMain` or `DllMain`) function.
    // Track it to give error messages for multiple entrypoints
    __gshared FuncDeclaration lastMain;

    /// Sibling nested functions which called this one
    FuncDeclarations siblingCallers;

    FuncDeclarations* inlinedNestedCallees;

    /// In case of failed `@safe` inference, store the error that made the function `@system` for
    /// better diagnostics
    AttributeViolation* safetyViolation;
    AttributeViolation* nogcViolation;
    AttributeViolation* pureViolation;
    AttributeViolation* nothrowViolation;

    /// See the `FUNCFLAG` struct
    import dmd.common.bitfields;
    mixin(generateBitFields!(FUNCFLAG, uint));

    /**
     * Data for a function declaration that is needed for the Objective-C
     * integration.
     */
    ObjcFuncDeclaration objc;

    extern (D) this(Loc loc, Loc endloc, Identifier ident, STC storage_class, Type type, bool noreturn = false)
    {
        super(DSYM.funcDeclaration, loc, ident);
        //.printf("FuncDeclaration(id = '%s', type = %s)\n", ident.toChars(), type.toChars());
        //.printf("storage_class = x%llx\n", storage_class);
        this.storage_class = storage_class;
        this.type = type;
        if (type)
        {
            // Normalize storage_class, because function-type related attributes
            // are already set in the 'type' in parsing phase.
            this.storage_class &= ~(STC.TYPECTOR | STC.FUNCATTR);
        }
        this.endloc = endloc;
        if (noreturn)
            this.noreturn = true;

        /* The type given for "infer the return type" is a TypeFunction with
         * NULL for the return type.
         */
        if (type && type.nextOf() is null)
            this.inferRetType = true;
    }

    static FuncDeclaration create(Loc loc, Loc endloc, Identifier id, StorageClass storage_class, Type type, bool noreturn = false)
    {
        return new FuncDeclaration(loc, endloc, id, cast(STC) storage_class, type, noreturn);
    }

    final nothrow pure @safe
    {
        private ref ContractInfo getContracts()
        {
            if (!contracts)
                contracts = new ContractInfo();
            return *contracts;
        }

        // getters
        inout(Statements*) frequires() inout { return contracts ? contracts.frequires : null; }
        inout(Ensures*) fensures() inout { return contracts ? contracts.fensures : null; }
        inout(Statement) frequire() inout { return contracts ? contracts.frequire: null; }
        inout(Statement) fensure() inout { return contracts ? contracts.fensure : null; }
        inout(FuncDeclaration) fdrequire() inout { return contracts ? contracts.fdrequire : null; }
        inout(FuncDeclaration) fdensure() inout { return contracts ? contracts.fdensure: null; }
        inout(Expressions*) fdrequireParams() inout { return contracts ? contracts.fdrequireParams: null; }
        inout(Expressions*) fdensureParams() inout { return contracts ? contracts.fdensureParams: null; }

        extern (D) private static string generateContractSetter(string field, string type)
        {
            return type ~ " " ~ field ~ "(" ~ type ~ " param)" ~
                    "{
                        if (!param && !contracts) return null;
                        return getContracts()." ~ field ~ " = param;
                     }";
        }

        mixin(generateContractSetter("frequires", "Statements*"));
        mixin(generateContractSetter("fensures", "Ensures*"));
        mixin(generateContractSetter("frequire", "Statement"));
        mixin(generateContractSetter("fensure", "Statement"));
        mixin(generateContractSetter("fdrequire", "FuncDeclaration"));
        mixin(generateContractSetter("fdensure", "FuncDeclaration"));
        mixin(generateContractSetter("fdrequireParams", "Expressions*"));
        mixin(generateContractSetter("fdensureParams", "Expressions*"));
    }

    override FuncDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("FuncDeclaration::syntaxCopy('%s')\n", toChars());
        FuncDeclaration f = s ? cast(FuncDeclaration)s
                              : new FuncDeclaration(loc, endloc, ident, storage_class, type.syntaxCopy(), this.noreturn != 0);
        f.frequires = frequires ? Statement.arraySyntaxCopy(frequires) : null;
        f.fensures = fensures ? Ensure.arraySyntaxCopy(fensures) : null;
        f.fbody = fbody ? fbody.syntaxCopy() : null;
        return f;
    }

    override final bool equals(const RootObject o) const
    {
        if (this == o)
            return true;

        auto s = isDsymbol(o);
        if (!s)
            return false;

        auto fd1 = this;
        auto fd2 = s.isFuncDeclaration();
        if (!fd2)
            return false;

        auto fa1 = fd1.isFuncAliasDeclaration();
        auto faf1 = fa1 ? fa1.toAliasFunc() : fd1;

        auto fa2 = fd2.isFuncAliasDeclaration();
        auto faf2 = fa2 ? fa2.toAliasFunc() : fd2;

        if (fa1 && fa2)
            return faf1.equals(faf2) && fa1.hasOverloads == fa2.hasOverloads;

        bool b1 = fa1 !is null;
        if (b1 && faf1.isUnique() && !fa1.hasOverloads)
            b1 = false;

        bool b2 = fa2 !is null;
        if (b2 && faf2.isUnique() && !fa2.hasOverloads)
            b2 = false;

        if (b1 != b2)
            return false;

        return faf1.toParent().equals(faf2.toParent()) &&
               faf1.ident.equals(faf2.ident) &&
               faf1.type.equals(faf2.type);
    }

    /****************************************************
     * Overload this FuncDeclaration with the new one f.
     * Return true if successful; i.e. no conflict.
     */
    override bool overloadInsert(Dsymbol s)
    {
        //printf("FuncDeclaration::overloadInsert(s = %s) this = %s\n", s.toChars(), toChars());
        assert(s != this);
        if (AliasDeclaration ad = s.isAliasDeclaration())
        {
            if (overnext)
                return overnext.overloadInsert(ad);
            if (!ad.aliassym && ad.type.ty != Tident && ad.type.ty != Tinstance && ad.type.ty != Ttypeof)
            {
                //printf("\tad = '%s'\n", ad.type.toChars());
                return false;
            }
            overnext = ad;
            //printf("\ttrue: no conflict\n");
            return true;
        }
        TemplateDeclaration td = s.isTemplateDeclaration();
        if (td)
        {
            if (!td.funcroot)
                td.funcroot = this;
            if (overnext)
                return overnext.overloadInsert(td);
            overnext = td;
            return true;
        }
        FuncDeclaration fd = s.isFuncDeclaration();
        if (!fd)
            return false;

        if (overnext)
        {
            td = overnext.isTemplateDeclaration();
            if (td)
                fd.overloadInsert(td);
            else
                return overnext.overloadInsert(fd);
        }
        overnext = fd;
        //printf("\ttrue: no conflict\n");
        return true;
    }

    /********************************************
     * find function template root in overload list
     */
    extern (D) final TemplateDeclaration findTemplateDeclRoot()
    {
        FuncDeclaration f = this;
        while (f && f.overnext)
        {
            //printf("f.overnext = %p %s\n", f.overnext, f.overnext.toChars());
            if (TemplateDeclaration td = f.overnext.isTemplateDeclaration())
                return td;
            f = f.overnext.isFuncDeclaration();
        }
        return null;
    }

    /********************************************
     * Returns true if function was declared
     * directly or indirectly in a unittest block
     */
    final bool inUnittest()
    {
        Dsymbol f = this;
        do
        {
            if (f.isUnitTestDeclaration())
                return true;
            f = f.toParent();
        }
        while (f);
        return false;
    }

    /********************************
     * Searches for a label with the given identifier. This function will insert a new
     * `LabelDsymbol` into `labtab` if it does not contain a mapping for `ident`.
     *
     * Params:
     *   ident = identifier of the requested label
     *   loc   = location used when creating a new `LabelDsymbol`
     *
     * Returns: the `LabelDsymbol` for `ident`
     */
    final LabelDsymbol searchLabel(Identifier ident, Loc loc)
    {
        Dsymbol s;
        if (!labtab)
            labtab = new DsymbolTable(); // guess we need one

        s = labtab.lookup(ident);
        if (!s)
        {
            s = new LabelDsymbol(ident, loc);
            labtab.insert(s);
        }
        return cast(LabelDsymbol)s;
    }

    /*****************************************
     * Determine lexical level difference from `this` to nested function `fd`.
     * Params:
     *      fd = target of call
     *      intypeof = !=0 if inside typeof
     * Returns:
     *      0       same level
     *      >0      decrease nesting by number
     *      -1      increase nesting by 1 (`fd` is nested within `this`)
     *      LevelError  error, `this` cannot call `fd`
     */
    extern (D) final int getLevel(FuncDeclaration fd, int intypeof)
    {
        //printf("FuncDeclaration::getLevel(fd = '%s')\n", fd.toChars());
        Dsymbol fdparent = fd.toParent2();
        if (fdparent == this)
            return -1;

        Dsymbol s = this;
        int level = 0;
        while (fd != s && fdparent != s.toParent2())
        {
            //printf("\ts = %s, '%s'\n", s.kind(), s.toChars());
            if (auto thisfd = s.isFuncDeclaration())
            {
                if (!thisfd.isNested() && !thisfd.vthis && !intypeof)
                    return LevelError;
            }
            else
            {
                if (auto thiscd = s.isAggregateDeclaration())
                {
                    /* AggregateDeclaration::isNested returns true only when
                     * it has a hidden pointer.
                     * But, calling the function belongs unrelated lexical scope
                     * is still allowed inside typeof.
                     *
                     * struct Map(alias fun) {
                     *   typeof({ return fun(); }) RetType;
                     *   // No member function makes Map struct 'not nested'.
                     * }
                     */
                    if (!thiscd.isNested() && !intypeof)
                        return LevelError;
                }
                else
                    return LevelError;
            }

            s = s.toParentP(fd);
            assert(s);
            level++;
        }
        return level;
    }

    enum LevelError = -2;

    override const(char)* toPrettyChars(bool QualifyTypes = false)
    {
        if (isMain())
            return "D main";
        return Dsymbol.toPrettyChars(QualifyTypes);
    }

    /** for diagnostics, e.g. 'int foo(int x, int y) pure' */
    final const(char)* toFullSignature()
    {
        OutBuffer buf;
        functionToBufferWithIdent(type.toTypeFunction(), buf, toChars(), isStatic);
        return buf.extractChars();
    }

    final bool isMain() const
    {
        return ident == Id.main && resolvedLinkage() != LINK.c && !isMember() && !isNested();
    }

    final bool isCMain() const
    {
        return ident == Id.main && resolvedLinkage() == LINK.c && !isMember() && !isNested();
    }

    final bool isWinMain() const
    {
        //printf("FuncDeclaration::isWinMain() %s\n", toChars());
        version (none)
        {
            bool x = ident == Id.WinMain && resolvedLinkage() != LINK.c && !isMember();
            printf("%s\n", x ? "yes" : "no");
            return x;
        }
        else
        {
            return ident == Id.WinMain && resolvedLinkage() != LINK.c && !isMember();
        }
    }

    final bool isDllMain() const
    {
        return ident == Id.DllMain && resolvedLinkage() != LINK.c && !isMember();
    }

    final bool isRtInit() const
    {
        return ident == Id.rt_init && resolvedLinkage() == LINK.c && !isMember() && !isNested();
    }

    override final bool isExport() const
    {
        return visibility.kind == Visibility.Kind.export_ || dllExport;
    }

    override final bool isImportedSymbol() const
    {
        //printf("isImportedSymbol()\n");
        //printf("protection = %d\n", visibility);
        return (visibility.kind == Visibility.Kind.export_ || dllImport) && !fbody;
    }

    override final bool isCodeseg() const pure nothrow @nogc @safe
    {
        return true; // functions are always in the code segment
    }

    override final bool isOverloadable() const
    {
        return true; // functions can be overloaded
    }

    /***********************************
     * Override so it can work even if semantic() hasn't yet
     * been run.
     */
    override final bool isAbstract()
    {
        if (storage_class & STC.abstract_)
            return true;
        if (semanticRun >= PASS.semanticdone)
            return false;

        if (_scope)
        {
           if (_scope.stc & STC.abstract_)
                return true;
           parent = _scope.parent;
           Dsymbol parent = toParent();
           if (parent.isInterfaceDeclaration())
                return true;
        }
        return false;
    }

    extern (D) final uint saveFlags()
    {
        return bitFields;
    }

    extern (D) final uint restoreFlags(uint f)
    {
        bitFields = f;
        return bitFields;
    }


    /**************************************
     * The function is doing something that may throw an exception, register that in case nothrow is being inferred
     *
     * Params:
     *     loc = location of action
     *     format = format string for error message
     *     args = arguments to format string
     */
    extern (D) final void setThrow(Loc loc, const(char)* format, RootObject[] args...)
    {
        if (nothrowInprocess && !nothrowViolation)
        {
            nothrowViolation = new AttributeViolation(loc, format, args); // action that requires GC
        }
    }

    /**************************************
     * The function calls non-`nothrow` function f, register that in case nothrow is being inferred
     * Params:
     *     loc = location of call
     *     fd = function being called
     */
    extern (D) final void setThrowCall(Loc loc, FuncDeclaration fd)
    {
        if (nothrowInprocess && !nothrowViolation)
        {
            nothrowViolation = new AttributeViolation(loc, fd); // action that requires GC
        }
    }

    /****************************************
     * Determine if function needs a static frame pointer.
     * Returns:
     *  `true` if function is really nested within other function.
     * Contracts:
     *  If isNested() returns true, isThis() should return false,
     *  unless the function needs a dual-context pointer.
     */
    bool isNested() const
    {
        auto f = toAliasFunc();
        //printf("\ttoParent2() = '%s'\n", f.toParent2().toChars());
        return ((f.storage_class & STC.static_) == 0) &&
                (f._linkage == LINK.d) &&
                (f.toParent2().isFuncDeclaration() !is null ||
                 f.toParent2() !is f.toParentLocal());
    }

    /****************************************
     * Determine if function is a non-static member function
     * that has an implicit 'this' expression.
     * Returns:
     *  The aggregate it is a member of, or null.
     * Contracts:
     *  Both isThis() and isNested() should return true if function needs a dual-context pointer,
     *  otherwise if isThis() returns true, isNested() should return false.
     */
    override inout(AggregateDeclaration) isThis() inout
    {
        //printf("+FuncDeclaration::isThis() '%s'\n", toChars());
        auto ad = (storage_class & STC.static_) ? .objc.isThis(this) : isMemberLocal();
        //printf("-FuncDeclaration::isThis() %p\n", ad);
        return ad;
    }

    override final bool needThis()
    {
        //printf("FuncDeclaration::needThis() '%s'\n", toChars());
        return toAliasFunc().isThis() !is null;
    }

    // Determine if a function is pedantically virtual
    final bool isVirtualMethod()
    {
        if (toAliasFunc() != this)
            return toAliasFunc().isVirtualMethod();

        //printf("FuncDeclaration::isVirtualMethod() %s\n", toChars());
        if (!isVirtual())
            return false;
        // If it's a final method, and does not override anything, then it is not virtual
        if (isFinalFunc() && foverrides.length == 0)
        {
            return false;
        }
        return true;
    }

    // Determine if function goes into virtual function pointer table
    bool isVirtual() const
    {
        if (toAliasFunc() != this)
            return toAliasFunc().isVirtual();

        auto p = toParent();

        if (!isMember || !p.isClassDeclaration)
            return false;

        if (p.isClassDeclaration.classKind == ClassKind.objc)
            return .objc.isVirtual(this);

        version (none)
        {
            printf("FuncDeclaration::isVirtual(%s)\n", toChars());
            printf("isMember:%p isStatic:%d private:%d ctor:%d !Dlinkage:%d\n", isMember(), isStatic(), visibility == Visibility.Kind.private_, isCtorDeclaration(), linkage != LINK.d);
            printf("result is %d\n", isMember() && !(isStatic() || visibility == Visibility.Kind.private_ || visibility == Visibility.Kind.package_) && p.isClassDeclaration() && !(p.isInterfaceDeclaration() && isFinalFunc()));
        }
        return !(isStatic() || visibility.kind == Visibility.Kind.private_ || visibility.kind == Visibility.Kind.package_) && !(p.isInterfaceDeclaration() && isFinalFunc());
    }

    final bool isFinalFunc() const
    {
        if (toAliasFunc() != this)
            return toAliasFunc().isFinalFunc();

        version (none)
        {{
            auto cd = toParent().isClassDeclaration();
            printf("FuncDeclaration::isFinalFunc(%s), %x\n", toChars(), Declaration.isFinal());
            printf("%p %d %d %d\n", isMember(), isStatic(), Declaration.isFinal(), ((cd = toParent().isClassDeclaration()) !is null && cd.storage_class & STC.final_));
            printf("result is %d\n", isMember() && (Declaration.isFinal() || (cd !is null && cd.storage_class & STC.final_)));
            if (cd)
                printf("\tmember of %s\n", cd.toChars());
        }}
        if (!isMember())
            return false;
        if (Declaration.isFinal())
            return true;
        auto cd = toParent().isClassDeclaration();
        return (cd !is null) && (cd.storage_class & STC.final_);
    }

    bool addPreInvariant()
    {
        auto ad = isThis();
        ClassDeclaration cd = ad ? ad.isClassDeclaration() : null;
        return (ad && !(cd && cd.isCPPclass()) && global.params.useInvariants == CHECKENABLE.on && (visibility.kind == Visibility.Kind.protected_ || visibility.kind == Visibility.Kind.public_ || visibility.kind == Visibility.Kind.export_) && !this.isNaked());
    }

    bool addPostInvariant()
    {
        auto ad = isThis();
        ClassDeclaration cd = ad ? ad.isClassDeclaration() : null;
        return (ad && !(cd && cd.isCPPclass()) && ad.inv && global.params.useInvariants == CHECKENABLE.on && (visibility.kind == Visibility.Kind.protected_ || visibility.kind == Visibility.Kind.public_ || visibility.kind == Visibility.Kind.export_) && !this.isNaked());
    }

    override const(char)* kind() const
    {
        return this.isGenerated() ? "generated function" : "function";
    }

    /********************************************
     * Returns:
     *  true if there are no overloads of this function
     */
    final bool isUnique() const
    {
        bool result = false;
        overloadApply(cast() this, (Dsymbol s)
        {
            auto f = s.isFuncDeclaration();
            auto td = s.isTemplateDeclaration();
            if (!f && !td)
                return 0;
            if (result)
            {
                result = false;
                return 1; // ambiguous, done
            }
            else
            {
                result = true;
                return 0;
            }
        });
        return result;
    }

    /*******************************
     * Look at all the variables in this function that are referenced
     * by nested functions, and determine if a closure needs to be
     * created for them.
     */
    final bool needsClosure()
    {
        /* Need a closure for all the closureVars[] if any of the
         * closureVars[] are accessed by a
         * function that escapes the scope of this function.
         * We take the conservative approach and decide that a function needs
         * a closure if it:
         * 1) is a virtual function
         * 2) has its address taken
         * 3) has a parent that escapes
         * 4) calls another nested function that needs a closure
         *
         * Note that since a non-virtual function can be called by
         * a virtual one, if that non-virtual function accesses a closure
         * var, the closure still has to be taken. Hence, we check for isThis()
         * instead of isVirtual(). (thanks to David Friedman)
         *
         * When the function returns a local struct or class, `requiresClosure`
         * is already set to `true` upon entering this function when the
         * struct/class refers to a local variable and a closure is needed.
         */
        //printf("FuncDeclaration::needsClosure() %s\n", toPrettyChars());

        if (requiresClosure)
            goto Lyes;

        for (size_t i = 0; i < closureVars.length; i++)
        {
            VarDeclaration v = closureVars[i];
            //printf("\tv = %s\n", v.toChars());

            for (size_t j = 0; j < v.nestedrefs.length; j++)
            {
                FuncDeclaration f = v.nestedrefs[j];
                assert(f != this);

                /* __require and __ensure will always get called directly,
                 * so they never make outer functions closure.
                 */
                if (f.ident == Id.require || f.ident == Id.ensure)
                    continue;

                //printf("\t\tf = %p, %s, isVirtual=%d, isThis=%p, tookAddressOf=%d\n", f, f.toChars(), f.isVirtual(), f.isThis(), f.tookAddressOf);

                /* Look to see if f escapes. We consider all parents of f within
                 * this, and also all siblings which call f; if any of them escape,
                 * so does f.
                 * Mark all affected functions as requiring closures.
                 */
                for (Dsymbol s = f; s && s != this; s = s.toParentP(this))
                {
                    FuncDeclaration fx = s.isFuncDeclaration();
                    if (!fx)
                        continue;
                    if (fx.isThis() || fx.tookAddressOf)
                    {
                        //printf("\t\tfx = %s, isVirtual=%d, isThis=%p, tookAddressOf=%d\n", fx.toChars(), fx.isVirtual(), fx.isThis(), fx.tookAddressOf);

                        /* Mark as needing closure any functions between this and f
                         */
                        markAsNeedingClosure((fx == f) ? fx.toParentP(this) : fx, this);

                        requiresClosure = true;
                    }

                    /* We also need to check if any sibling functions that
                     * called us, have escaped. This is recursive: we need
                     * to check the callers of our siblings.
                     */
                    if (checkEscapingSiblings(fx, this))
                        requiresClosure = true;

                    /* https://issues.dlang.org/show_bug.cgi?id=12406
                     * Iterate all closureVars to mark all descendant
                     * nested functions that access to the closing context of this function.
                     */
                }
            }
        }
        if (requiresClosure)
            goto Lyes;

        return false;

    Lyes:
        return true;
    }

    /***********************************************
     * Determine if function's variables are referenced by a function
     * nested within it.
     */
    final bool hasNestedFrameRefs()
    {
        if (closureVars.length)
            return true;

        /* If a virtual function has contracts, assume its variables are referenced
         * by those contracts, even if they aren't. Because they might be referenced
         * by the overridden or overriding function's contracts.
         * This can happen because frequire and fensure are implemented as nested functions,
         * and they can be called directly by an overriding function and the overriding function's
         * context had better match, or
         * https://issues.dlang.org/show_bug.cgi?id=7335 will bite.
         */
        if (fdrequire || fdensure)
            return true;

        if (foverrides.length && isVirtualMethod())
        {
            for (size_t i = 0; i < foverrides.length; i++)
            {
                FuncDeclaration fdv = foverrides[i];
                if (fdv.hasNestedFrameRefs())
                    return true;
            }
        }
        return false;
    }

    /*********************************************
     * Returns: the function's parameter list, and whether
     * it is variadic or not.
     */
    final ParameterList getParameterList()
    {
        if (type)
        {
            if (TypeFunction fdtype = type.isTypeFunction()) // Could also be TypeError
                return fdtype.parameterList;
        }

        return ParameterList(null, VarArg.none);
    }

    /**********************************
     * Generate a FuncDeclaration for a runtime library function.
     */
    extern(D) static FuncDeclaration genCfunc(Parameters* fparams, Type treturn, const(char)* name, STC stc = STC.none)
    {
        return genCfunc(fparams, treturn, Identifier.idPool(name[0 .. strlen(name)]), stc);
    }

    extern(D) static FuncDeclaration genCfunc(Parameters* fparams, Type treturn, Identifier id, STC stc = STC.none)
    {
        FuncDeclaration fd;
        TypeFunction tf;
        Dsymbol s;
        __gshared DsymbolTable st = null;

        //printf("genCfunc(name = '%s')\n", id.toChars());
        //printf("treturn\n\t"); treturn.print();

        // See if already in table
        if (!st)
            st = new DsymbolTable();
        s = st.lookup(id);
        if (s)
        {
            fd = s.isFuncDeclaration();
            assert(fd);
            assert(fd.type.nextOf().equals(treturn));
        }
        else
        {
            tf = new TypeFunction(ParameterList(fparams), treturn, LINK.c, stc);
            fd = new FuncDeclaration(Loc.initial, Loc.initial, id, STC.static_, tf);
            fd.visibility = Visibility(Visibility.Kind.public_);
            fd._linkage = LINK.c;

            st.insert(fd);
        }
        return fd;
    }

    inout(FuncDeclaration) toAliasFunc() inout @safe
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/**
Checks for mismatching modifiers between `lhsMod` and `rhsMod` and prints the
mismatching modifiers to `buf`.

The modifiers of the `lhsMod` mismatching the ones with the `rhsMod` are printed, i.e.
lhs(shared) vs. rhs() prints "`shared`", wheras lhs() vs rhs(shared) prints "non-shared".

Params:
    buf = output buffer to write to
    lhsMod = modifier on the left-hand side
    lhsMod = modifier on the right-hand side

Returns:

A tuple with `isMutable` and `isNotShared` set
if the `lhsMod` is missing those modifiers (compared to rhs).
*/
auto MODMatchToBuffer(OutBuffer* buf, ubyte lhsMod, ubyte rhsMod)
{
    static struct Mismatches
    {
        bool isNotShared;
        bool isMutable;
    }

    Mismatches mismatches;

    bool bothMutable = ((lhsMod & rhsMod) == 0);
    bool sharedMismatch = ((lhsMod ^ rhsMod) & MODFlags.shared_) != 0;
    bool sharedMismatchOnly = ((lhsMod ^ rhsMod) == MODFlags.shared_);

    if (lhsMod & MODFlags.shared_)
        buf.writestring("`shared` ");
    else if (sharedMismatch && !(lhsMod & MODFlags.immutable_))
    {
        buf.writestring("non-shared ");
        mismatches.isNotShared = true;
    }

    if (bothMutable && sharedMismatchOnly)
    {
    }
    else if (lhsMod & MODFlags.immutable_)
        buf.writestring("`immutable` ");
    else if (lhsMod & MODFlags.const_)
        buf.writestring("`const` ");
    else if (lhsMod & MODFlags.wild)
        buf.writestring("`inout` ");
    else
    {
        buf.writestring("mutable ");
        mismatches.isMutable = true;
    }

    return mismatches;
}

///
unittest
{
    OutBuffer buf;
    auto mismatches = MODMatchToBuffer(&buf, MODFlags.shared_, 0);
    assert(buf[] == "`shared` ");
    assert(!mismatches.isNotShared);

    buf.setsize(0);
    mismatches = MODMatchToBuffer(&buf, 0, MODFlags.shared_);
    assert(buf[] == "non-shared ");
    assert(mismatches.isNotShared);

    buf.setsize(0);
    mismatches = MODMatchToBuffer(&buf, MODFlags.const_, 0);
    assert(buf[] == "`const` ");
    assert(!mismatches.isMutable);

    buf.setsize(0);
    mismatches = MODMatchToBuffer(&buf, 0, MODFlags.const_);
    assert(buf[] == "mutable ");
    assert(mismatches.isMutable);
}

/* For all functions between outerFunc and f, mark them as needing
 * a closure.
 */
private void markAsNeedingClosure(Dsymbol f, FuncDeclaration outerFunc)
{
    for (Dsymbol sx = f; sx && sx != outerFunc; sx = sx.toParentP(outerFunc))
    {
        FuncDeclaration fy = sx.isFuncDeclaration();
        if (fy && fy.closureVars.length)
        {
            /* fy needs a closure if it has closureVars[],
             * because the frame pointer in the closure will be accessed.
             */
            fy.requiresClosure = true;
        }
    }
}

/********
 * Given a nested function f inside a function outerFunc, check
 * if any sibling callers of f have escaped. If so, mark
 * all the enclosing functions as needing closures.
 * This is recursive: we need to check the callers of our siblings.
 * Note that nested functions can only call lexically earlier nested
 * functions, so loops are impossible.
 * Params:
 *      f = inner function (nested within outerFunc)
 *      outerFunc = outer function
 *      p = for internal recursion use
 * Returns:
 *      true if any closures were needed
 */
bool checkEscapingSiblings(FuncDeclaration f, FuncDeclaration outerFunc, void* p = null)
{
    static struct PrevSibling
    {
        PrevSibling* p;
        FuncDeclaration f;
    }

    if (f.computedEscapingSiblings)
        return f.hasEscapingSiblings;

    PrevSibling ps;
    ps.p = cast(PrevSibling*)p;
    ps.f = f;

    //printf("checkEscapingSiblings(f = %s, outerfunc = %s)\n", f.toChars(), outerFunc.toChars());
    bool bAnyClosures = false;
    for (size_t i = 0; i < f.siblingCallers.length; ++i)
    {
        FuncDeclaration g = f.siblingCallers[i];
        if (g.isThis() || g.tookAddressOf)
        {
            markAsNeedingClosure(g, outerFunc);
            bAnyClosures = true;
        }

        for (auto parent = g.toParentP(outerFunc); parent && parent !is outerFunc; parent = parent.toParentP(outerFunc))
        {
            // A parent of the sibling had its address taken.
            // Assume escaping of parent affects its children, so needs propagating.
            // see https://issues.dlang.org/show_bug.cgi?id=19679
            FuncDeclaration parentFunc = parent.isFuncDeclaration;
            if (parentFunc && parentFunc.tookAddressOf)
            {
                markAsNeedingClosure(parentFunc, outerFunc);
                bAnyClosures = true;
            }
        }

        PrevSibling* prev = cast(PrevSibling*)p;
        while (1)
        {
            if (!prev)
            {
                bAnyClosures |= checkEscapingSiblings(g, outerFunc, &ps);
                break;
            }
            if (prev.f == g)
                break;
            prev = prev.p;
        }
    }
    f.hasEscapingSiblings = bAnyClosures;
    f.computedEscapingSiblings = true;
    //printf("\t%d\n", bAnyClosures);
    return bAnyClosures;
}

/***********************************************************
 * Used as a way to import a set of functions from another scope into this one.
 */
extern (C++) final class FuncAliasDeclaration : FuncDeclaration
{
    FuncDeclaration funcalias;
    bool hasOverloads;

    extern (D) this(Identifier ident, FuncDeclaration funcalias, bool hasOverloads = true)
    {
        super(funcalias.loc, funcalias.endloc, ident, funcalias.storage_class, funcalias.type);
        assert(funcalias != this);
        this.funcalias = funcalias;
        this.dsym = DSYM.funcAliasDeclaration;

        this.hasOverloads = hasOverloads;
        if (hasOverloads)
        {
            if (FuncAliasDeclaration fad = funcalias.isFuncAliasDeclaration())
                this.hasOverloads = fad.hasOverloads;
        }
        else
        {
            // for internal use
            assert(!funcalias.isFuncAliasDeclaration());
            this.hasOverloads = false;
        }
        userAttribDecl = funcalias.userAttribDecl;
    }

    override const(char)* kind() const
    {
        return "function alias";
    }

    override inout(FuncDeclaration) toAliasFunc() inout
    {
        return funcalias.toAliasFunc();
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class FuncLiteralDeclaration : FuncDeclaration
{
    TOK tok;        // TOK.function_ or TOK.delegate_
    Type treq;      // target of return type inference

    // backend
    bool deferToObj;

    extern (D) this(Loc loc, Loc endloc, Type type, TOK tok, ForeachStatement fes, Identifier id = null, STC storage_class = STC.none)
    {
        super(loc, endloc, null, storage_class, type);
        this.dsym = DSYM.funcLiteralDeclaration;
        this.ident = id ? id : Id.empty;
        this.tok = tok;
        this.fes = fes;
        // Always infer scope for function literals
        // See https://issues.dlang.org/show_bug.cgi?id=20362
        this.scopeInprocess = true;
        //printf("FuncLiteralDeclaration() id = '%s', type = '%s'\n", this.ident.toChars(), type.toChars());
    }

    override FuncLiteralDeclaration syntaxCopy(Dsymbol s)
    {
        //printf("FuncLiteralDeclaration::syntaxCopy('%s')\n", toChars());
        assert(!s);
        auto f = new FuncLiteralDeclaration(loc, endloc, type.syntaxCopy(), tok, fes, ident, storage_class & STC.auto_);
        f.treq = treq; // don't need to copy
        FuncDeclaration.syntaxCopy(f);
        return f;
    }

    override bool isNested() const
    {
        //printf("FuncLiteralDeclaration::isNested() '%s'\n", toChars());
        return (tok != TOK.function_) && !isThis();
    }

    override inout(AggregateDeclaration) isThis() inout
    {
        return tok == TOK.delegate_ ? super.isThis() : null;
    }

    override bool isVirtual() const
    {
        return false;
    }

    override bool addPreInvariant()
    {
        return false;
    }

    override bool addPostInvariant()
    {
        return false;
    }

    override const(char)* kind() const
    {
        // GCC requires the (char*) casts
        return (tok != TOK.function_) ? "delegate" : "function";
    }

    override const(char)* toPrettyChars(bool QualifyTypes = false)
    {
        if (parent)
        {
            if (TemplateInstance ti = parent.isTemplateInstance())
                return ti.tempdecl.toPrettyChars(QualifyTypes);
        }
        return Dsymbol.toPrettyChars(QualifyTypes);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class CtorDeclaration : FuncDeclaration
{
    bool isCpCtor;    // copy constructor
    bool isMoveCtor;  // move constructor (aka rvalue constructor)
    extern (D) this(Loc loc, Loc endloc, STC stc, Type type)
    {
        super(loc, endloc, Id.ctor, stc, type);
        this.dsym = DSYM.ctorDeclaration;
        //printf("CtorDeclaration(loc = %s) %s %p\n", loc.toChars(), toChars(), this);
    }

    override CtorDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto f = new CtorDeclaration(loc, endloc, storage_class, type.syntaxCopy());
        FuncDeclaration.syntaxCopy(f);
        return f;
    }

    override const(char)* kind() const
    {
        return isCpCtor ? "copy constructor" : "constructor";
    }

    override bool isVirtual() const
    {
        return false;
    }

    override bool addPreInvariant()
    {
        return false;
    }

    override bool addPostInvariant()
    {
        return (isThis() && vthis && global.params.useInvariants == CHECKENABLE.on);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class PostBlitDeclaration : FuncDeclaration
{
    extern (D) this(Loc loc, Loc endloc, STC stc, Identifier id)
    {
        super(loc, endloc, id, stc, null);
        this.dsym = DSYM.postBlitDeclaration;
    }

    override PostBlitDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto dd = new PostBlitDeclaration(loc, endloc, storage_class, ident);
        FuncDeclaration.syntaxCopy(dd);
        return dd;
    }

    override bool isVirtual() const
    {
        return false;
    }

    override bool addPreInvariant()
    {
        return false;
    }

    override bool addPostInvariant()
    {
        return (isThis() && vthis && global.params.useInvariants == CHECKENABLE.on);
    }

    override bool overloadInsert(Dsymbol s)
    {
        return false; // cannot overload postblits
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class DtorDeclaration : FuncDeclaration
{
    extern (D) this(Loc loc, Loc endloc)
    {
        super(loc, endloc, Id.dtor, STC.none, null);
        this.dsym = DSYM.dtorDeclaration;
    }

    extern (D) this(Loc loc, Loc endloc, STC stc, Identifier id)
    {
        super(loc, endloc, id, stc, null);
        this.dsym = DSYM.dtorDeclaration;
    }

    override DtorDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto dd = new DtorDeclaration(loc, endloc, storage_class, ident);
        FuncDeclaration.syntaxCopy(dd);
        return dd;
    }

    override const(char)* kind() const
    {
        return "destructor";
    }

    override bool isVirtual() const
    {
        // D dtor's don't get put into the vtbl[]
        // this is a hack so that extern(C++) destructors report as virtual, which are manually added to the vtable
        return vtblIndex != -1;
    }

    override bool addPreInvariant()
    {
        return (isThis() && vthis && global.params.useInvariants == CHECKENABLE.on);
    }

    override bool addPostInvariant()
    {
        return false;
    }

    override bool overloadInsert(Dsymbol s)
    {
        return false; // cannot overload destructors
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) class StaticCtorDeclaration : FuncDeclaration
{
    extern (D) this(Loc loc, Loc endloc, STC stc)
    {
        super(loc, endloc, Identifier.generateIdWithLoc("_staticCtor", loc), STC.static_ | stc, null);
        this.dsym = DSYM.staticCtorDeclaration;
    }

    extern (D) this(Loc loc, Loc endloc, string name, STC stc)
    {
        super(loc, endloc, Identifier.generateIdWithLoc(name, loc), STC.static_ | stc, null);
        this.dsym = DSYM.staticCtorDeclaration;
    }

    override StaticCtorDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto scd = new StaticCtorDeclaration(loc, endloc, storage_class);
        FuncDeclaration.syntaxCopy(scd);
        return scd;
    }

    override final inout(AggregateDeclaration) isThis() inout @nogc nothrow pure @safe
    {
        return null;
    }

    override final bool isVirtual() const @nogc nothrow pure @safe
    {
        return false;
    }

    override final bool addPreInvariant() @nogc nothrow pure @safe
    {
        return false;
    }

    override final bool addPostInvariant() @nogc nothrow pure @safe
    {
        return false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class SharedStaticCtorDeclaration : StaticCtorDeclaration
{
    /// Exclude this constructor from cyclic dependency check
    bool standalone;

    extern (D) this(Loc loc, Loc endloc, STC stc)
    {
        super(loc, endloc, "_sharedStaticCtor", stc);
        this.dsym = DSYM.sharedStaticCtorDeclaration;
    }

    override SharedStaticCtorDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto scd = new SharedStaticCtorDeclaration(loc, endloc, storage_class);
        FuncDeclaration.syntaxCopy(scd);
        return scd;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) class StaticDtorDeclaration : FuncDeclaration
{
    VarDeclaration vgate; // 'gate' variable

    extern (D) this(Loc loc, Loc endloc, STC stc)
    {
        super(loc, endloc, Identifier.generateIdWithLoc("_staticDtor", loc), STC.static_ | stc, null);
        this.dsym = DSYM.staticDtorDeclaration;
    }

    extern (D) this(Loc loc, Loc endloc, string name, STC stc)
    {
        super(loc, endloc, Identifier.generateIdWithLoc(name, loc), STC.static_ | stc, null);
        this.dsym = DSYM.staticDtorDeclaration;
    }

    override StaticDtorDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto sdd = new StaticDtorDeclaration(loc, endloc, storage_class);
        FuncDeclaration.syntaxCopy(sdd);
        return sdd;
    }

    override final inout(AggregateDeclaration) isThis() inout
    {
        return null;
    }

    override final bool isVirtual() const
    {
        return false;
    }

    override final bool addPreInvariant()
    {
        return false;
    }

    override final bool addPostInvariant()
    {
        return false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class SharedStaticDtorDeclaration : StaticDtorDeclaration
{
    extern (D) this(Loc loc, Loc endloc, STC stc)
    {
        super(loc, endloc, "_sharedStaticDtor", stc);
        this.dsym = DSYM.sharedStaticDtorDeclaration;
    }

    override SharedStaticDtorDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto sdd = new SharedStaticDtorDeclaration(loc, endloc, storage_class);
        FuncDeclaration.syntaxCopy(sdd);
        return sdd;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class InvariantDeclaration : FuncDeclaration
{
    extern (D) this(Loc loc, Loc endloc, STC stc, Identifier id, Statement fbody)
    {
        // Make a unique invariant for now; we'll fix it up as we add it to the aggregate invariant list.
        super(loc, endloc, id ? id : Identifier.generateId("__invariant"), stc, null);
        this.fbody = fbody;
        this.dsym = DSYM.invariantDeclaration;
    }

    override InvariantDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto id = new InvariantDeclaration(loc, endloc, storage_class, null, null);
        FuncDeclaration.syntaxCopy(id);
        return id;
    }

    override bool isVirtual() const
    {
        return false;
    }

    override bool addPreInvariant()
    {
        return false;
    }

    override bool addPostInvariant()
    {
        return false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    extern (D) void fixupInvariantIdent(size_t offset)
    {
        OutBuffer idBuf;
        idBuf.writestring("__invariant");
        idBuf.print(offset);

        ident = Identifier.idPool(idBuf[]);
    }
}


/***********************************************************
 */
extern (C++) final class UnitTestDeclaration : FuncDeclaration
{
    char* codedoc;      // for documented unittest

    // toObjFile() these nested functions after this one
    FuncDeclarations deferredNested;

    extern (D) this(Loc loc, Loc endloc, STC stc, char* codedoc)
    {
        super(loc, endloc, Identifier.generateIdWithLoc("__unittest", loc), stc, null);
        this.codedoc = codedoc;
        this.dsym = DSYM.unitTestDeclaration;
    }

    override UnitTestDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto utd = new UnitTestDeclaration(loc, endloc, storage_class, codedoc);
        FuncDeclaration.syntaxCopy(utd);
        return utd;
    }

    override inout(AggregateDeclaration) isThis() inout
    {
        return null;
    }

    override bool isVirtual() const
    {
        return false;
    }

    override bool addPreInvariant()
    {
        return false;
    }

    override bool addPostInvariant()
    {
        return false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class NewDeclaration : FuncDeclaration
{
    extern (D) this(Loc loc, STC stc)
    {
        super(loc, Loc.initial, Id.classNew, STC.static_ | stc, null);
        this.dsym = DSYM.newDeclaration;
    }

    override NewDeclaration syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto f = new NewDeclaration(loc, storage_class);
        FuncDeclaration.syntaxCopy(f);
        return f;
    }

    override const(char)* kind() const
    {
        return "allocator";
    }

    override bool isVirtual() const
    {
        return false;
    }

    override bool addPreInvariant()
    {
        return false;
    }

    override bool addPostInvariant()
    {
        return false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/// Stores a reason why a function failed to infer a function attribute like `@safe` or `pure`
///
/// Has two modes:
/// - a regular safety error, stored in `action`
/// - a call to a function without the attribute, which is a special case, because in that case,
///   that function might recursively also have a `AttributeViolation`. This way, in case
///   of a big call stack, the error can go down all the way to the root cause.
struct AttributeViolation
{
    Loc loc;               /// location of error

    FuncDeclaration fd;    /// function is the focus of the violation

    // -- OR --

    string action;   /// Action that made the attribute fail to get inferred

    this(Loc loc, FuncDeclaration fd) { this.loc = loc; this.fd = fd; }

    this(Loc loc, const(char)* fmt, RootObject[] args)
    {
        this.loc = loc;
        assert(args.length <= 4); // expand if necessary
        OutBuffer buf;
        buf.printf(fmt,
            args.length > 0 && args[0] ? args[0].toErrMsg() : "",
            args.length > 1 && args[1] ? args[1].toErrMsg() : "",
            args.length > 2 && args[2] ? args[2].toErrMsg() : "",
            args.length > 3 && args[3] ? args[3].toErrMsg() : "",
        );
        this.action = buf.extractSlice();
    }
}
