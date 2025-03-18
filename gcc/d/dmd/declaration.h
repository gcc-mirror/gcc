
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/declaration.h
 */

#pragma once

#include "dsymbol.h"
#include "mtype.h"
#include "objc.h"
#include "tokens.h"

class Expression;
class Statement;
class LabelDsymbol;
class Initializer;
class ForeachStatement;
struct Ensure
{
    Identifier *id;
    Statement *ensure;
};
class FuncDeclaration;
class StructDeclaration;
struct IntRange;
struct AttributeViolation;

namespace dmd
{
    bool functionSemantic(FuncDeclaration* fd);
    bool functionSemantic3(FuncDeclaration* fd);
    bool checkClosure(FuncDeclaration* fd);
    MATCH leastAsSpecialized(FuncDeclaration *f, FuncDeclaration *g, Identifiers *names);
    PURE isPure(FuncDeclaration *f);
    FuncDeclaration *genCfunc(Parameters *args, Type *treturn, const char *name, StorageClass stc=0);
    FuncDeclaration *genCfunc(Parameters *args, Type *treturn, Identifier *id, StorageClass stc=0);
}

//enum STC : ulong from astenums.d:

    #define STCundefined          0ULL

    #define STCstatic             1ULL    /// `static`
    #define STCextern             2ULL    /// `extern`
    #define STCconst              4ULL    /// `const`
    #define STCfinal              8ULL    /// `final`

    #define STCabstract           0x10ULL    /// `abstract`
    #define STCparameter          0x20ULL    /// is function parameter
    #define STCfield              0x40ULL    /// is field of struct, union or class
    #define STCoverride           0x80ULL    /// `override`

    #define STCauto               0x100ULL    /// `auto`
    #define STCsynchronized       0x200ULL    /// `synchronized`
    #define STCdeprecated         0x400ULL    /// `deprecated`
    #define STCin                 0x800ULL    /// `in` parameter

    #define STCout                0x1000ULL    /// `out` parameter
    #define STClazy               0x2000ULL    /// `lazy` parameter
    #define STCforeach            0x4000ULL    /// variable for foreach loop
    #define STCvariadic           0x8000ULL    /// the `variadic` parameter in: T foo(T a, U b, V variadic...)

    #define STCconstscoperef      0x10000ULL    /// when `in` means const|scope|ref
    #define STCtemplateparameter  0x20000ULL    /// template parameter
    #define STCref                0x40000ULL    /// `ref`
    #define STCscope              0x80000ULL    /// `scope`

    #define STCscopeinferred      0x200000ULL    /// `scope` has been inferred and should not be part of mangling, `scope` must also be set
    #define STCreturn             0x400000ULL    /// 'return ref' or 'return scope' for function parameters
    #define STCreturnScope        0x800000ULL    /// if `ref return scope` then resolve to `ref` and `return scope`

    #define STCreturninferred     0x1000000ULL    /// `return` has been inferred and should not be part of mangling, `return` must also be set
    #define STCimmutable          0x2000000ULL    /// `immutable`
    //                            0x4000000ULL
    #define STCmanifest           0x8000000ULL    /// manifest constant

    #define STCnodtor             0x10000000ULL    /// do not run destructor
    #define STCnothrow            0x20000000ULL    /// `nothrow` meaning never throws exceptions
    #define STCpure               0x40000000ULL    /// `pure` function

    #define STCalias              0x100000000ULL    /// `alias` parameter
    #define STCshared             0x200000000ULL    /// accessible from multiple threads
    #define STCgshared            0x400000000ULL    /// accessible from multiple threads, but not typed as `shared`
    #define STCwild               0x800000000ULL    /// for wild type constructor

    #define STCproperty           0x1000000000ULL    /// `@property`
    #define STCsafe               0x2000000000ULL    /// `@safe`
    #define STCtrusted            0x4000000000ULL    /// `@trusted`
    #define STCsystem             0x8000000000ULL    /// `@system`

    #define STCctfe               0x10000000000ULL    /// can be used in CTFE, even if it is static
    #define STCdisable            0x20000000000ULL    /// for functions that are not callable
    #define STCresult             0x40000000000ULL    /// for result variables passed to out contracts
    #define STCnodefaultctor      0x80000000000ULL    /// must be set inside constructor

    #define STCtemp               0x100000000000ULL    /// temporary variable
    #define STCrvalue             0x200000000000ULL    /// force rvalue for variables
    #define STCnogc               0x400000000000ULL    /// `@nogc`
    #define STCautoref            0x800000000000ULL    /// Mark for the already deduced `auto ref` parameter

    #define STCinference          0x1000000000000ULL    /// do attribute inference
    #define STCexptemp            0x2000000000000ULL    /// temporary variable that has lifetime restricted to an expression
    #define STCfuture             0x4000000000000ULL    /// introducing new base class function
    #define STClocal              0x8000000000000ULL    /// do not forward (see dmd.dsymbol.ForwardingScopeDsymbol).

    #define STClive               0x10000000000000ULL    /// function `@live` attribute
    #define STCregister           0x20000000000000ULL    /// `register` storage class (ImportC)
    #define STCvolatile           0x40000000000000ULL    /// destined for volatile in the back end

#define STC_TYPECTOR    (STCconst | STCimmutable | STCshared | STCwild)
#define STC_FUNCATTR    (STCref | STCnothrow | STCnogc | STCpure | STCproperty | STCsafe | STCtrusted | STCsystem)

/**************************************************************/

class Declaration : public Dsymbol
{
public:
    Type *type;
    Type *originalType;         // before semantic analysis
    StorageClass storage_class;
    DString mangleOverride;     // overridden symbol with pragma(mangle, "...")
    Visibility visibility;
    short inuse;                // used to detect cycles
    uint8_t bitFields;

    LINK _linkage() const;
    LINK _linkage(LINK v);
    bool noUnderscore() const;

    const char *kind() const override;
    uinteger_t size(Loc loc) override final;


    bool isStatic() const { return (storage_class & STCstatic) != 0; }
    LINK resolvedLinkage() const; // returns the linkage, resolving the target-specific `System` one
    virtual bool isDelete();
    virtual bool isDataseg();
    virtual bool isThreadlocal();
    virtual bool isCodeseg() const;
    bool isFinal() const        { return (storage_class & STCfinal) != 0; }
    virtual bool isAbstract()   { return (storage_class & STCabstract) != 0; }
    bool isConst() const        { return (storage_class & STCconst) != 0; }
    bool isImmutable() const    { return (storage_class & STCimmutable) != 0; }
    bool isWild() const         { return (storage_class & STCwild) != 0; }
    bool isAuto() const         { return (storage_class & STCauto) != 0; }
    bool isScope() const        { return (storage_class & STCscope) != 0; }
    bool isReturn() const       { return (storage_class & STCreturn) != 0; }
    bool isSynchronized() const { return (storage_class & STCsynchronized) != 0; }
    bool isParameter() const    { return (storage_class & STCparameter) != 0; }
    bool isDeprecated() const override final { return (storage_class & STCdeprecated) != 0; }
    bool isOverride() const     { return (storage_class & STCoverride) != 0; }
    bool isResult() const       { return (storage_class & STCresult) != 0; }
    bool isField() const        { return (storage_class & STCfield) != 0; }

    bool isIn()  const  { return (storage_class & STCin) != 0; }
    bool isOut() const  { return (storage_class & STCout) != 0; }
    bool isRef() const  { return (storage_class & STCref) != 0; }
    bool isReference() const { return (storage_class & (STCref | STCout)) != 0; }

    bool isFuture() const { return (storage_class & STCfuture) != 0; }

    Visibility visible() override final;

    void accept(Visitor *v) override { v->visit(this); }
};

/**************************************************************/

class TupleDeclaration final : public Declaration
{
public:
    Objects *objects;
    TypeTuple *tupletype;       // !=NULL if this is a type tuple
    d_bool isexp;                 // true: expression tuple
    d_bool building;              // it's growing in AliasAssign semantic

    TupleDeclaration *syntaxCopy(Dsymbol *) override;
    const char *kind() const override;
    Type *getType() override;
    Dsymbol *toAlias2() override;
    bool needThis() override;

    void accept(Visitor *v) override { v->visit(this); }
};

/**************************************************************/

class AliasDeclaration final : public Declaration
{
public:
    Dsymbol *aliassym;
    Dsymbol *overnext;          // next in overload list
    Dsymbol *_import;           // !=NULL if unresolved internal alias for selective import

    static AliasDeclaration *create(Loc loc, Identifier *id, Type *type);
    AliasDeclaration *syntaxCopy(Dsymbol *) override;
    bool overloadInsert(Dsymbol *s) override;
    const char *kind() const override;
    Type *getType() override;
    Dsymbol *toAlias() override;
    Dsymbol *toAlias2() override;
    bool isOverloadable() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

/**************************************************************/

class OverDeclaration final : public Declaration
{
public:
    Dsymbol *overnext;          // next in overload list
    Dsymbol *aliassym;

    const char *kind() const override;
    bool equals(const RootObject * const o) const override;
    bool overloadInsert(Dsymbol *s) override;

    Dsymbol *toAlias() override;
    Dsymbol *isUnique();
    bool isOverloadable() const override;

    void accept(Visitor *v) override { v->visit(this); }
};

/**************************************************************/

class VarDeclaration : public Declaration
{
public:
    Initializer *_init;
    FuncDeclarations nestedrefs; // referenced by these lexically nested functions
    TupleDeclaration *aliasTuple;  // if `this` is really a tuple of declarations
    VarDeclaration *lastVar;    // Linked list of variables for goto-skips-init detection
    Expression *edtor;          // if !=NULL, does the destruction of the variable
    IntRange *range;            // if !NULL, the variable is known to be within the range

    unsigned endlinnum;         // line number of end of scope that this var lives in
    unsigned offset;
    unsigned sequenceNumber;     // order the variables are declared
    structalign_t alignment;

    // When interpreting, these point to the value (NULL if value not determinable)
    // The index of this variable on the CTFE stack, ~0u if not allocated
    unsigned ctfeAdrOnStack;
private:
    uint32_t bitFields;
public:
    int8_t canassign; // // it can be assigned to
    uint8_t isdataseg; // private data for isDataseg
    bool isargptr() const; // if parameter that _argptr points to
    bool isargptr(bool v);
    bool ctorinit() const; // it has been initialized in a ctor
    bool ctorinit(bool v);
    bool iscatchvar() const; // this is the exception object variable in catch() clause
    bool iscatchvar(bool v);
    bool isowner() const; // this is an Owner, despite it being `scope`
    bool isowner(bool v);
    bool setInCtorOnly() const; // field can only be set in a constructor, as it is const or immutable
    bool setInCtorOnly(bool v);
    bool onstack() const; // it is a class that was allocated on the stack
    bool onstack(bool v);
    bool overlapped() const; // if it is a field and has overlapping
    bool overlapped(bool v);
    bool overlapUnsafe() const; // if it is an overlapping field and the overlaps are unsafe
    bool overlapUnsafe(bool v);
    bool maybeScope() const; // allow inferring 'scope' for this variable
    bool maybeScope(bool v);
    bool doNotInferReturn() const; // do not infer 'return' for this variable
    bool doNotInferReturn(bool v);
    bool isArgDtorVar() const; // temporary created to handle scope destruction of a function argument
    bool isArgDtorVar(bool v);
    bool isCmacro() const; // if a C macro turned into a C variable
    bool isCmacro(bool v);
#if MARS
    bool inClosure() const; // is inserted into a GC allocated closure
    bool inClosure(bool v);
    bool inAlignSection() const; // is inserted into aligned section on stack
    bool inAlignSection(bool v);
#endif
    bool systemInferred() const;
    bool systemInferred(bool v);
    static VarDeclaration *create(Loc loc, Type *t, Identifier *id, Initializer *init, StorageClass storage_class = STCundefined);
    VarDeclaration *syntaxCopy(Dsymbol *) override;
    const char *kind() const override;
    AggregateDeclaration *isThis() override final;
    bool needThis() override final;
    bool isExport() const override final;
    bool isImportedSymbol() const override final;
    bool isCtorinit() const;
    bool isDataseg() override final;
    bool isThreadlocal() override final;
    bool isCTFE();
    bool isOverlappedWith(VarDeclaration *v);
    bool hasPointers() override final;
    bool canTakeAddressOf();
    bool needsScopeDtor();
    Dsymbol *toAlias() override final;
    // Eliminate need for dynamic_cast
    void accept(Visitor *v) override { v->visit(this); }
};

/**************************************************************/

class BitFieldDeclaration : public VarDeclaration
{
public:
    Expression *width;

    unsigned fieldWidth;
    unsigned bitOffset;

    BitFieldDeclaration *syntaxCopy(Dsymbol *) override;
    void accept(Visitor *v) override { v->visit(this); }
};

/**************************************************************/

// This is a shell around a back end symbol

class SymbolDeclaration final : public Declaration
{
public:
    AggregateDeclaration *dsym;

    // Eliminate need for dynamic_cast
    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoDeclaration : public VarDeclaration
{
public:
    Type *tinfo;

    static TypeInfoDeclaration *create(Type *tinfo);
    TypeInfoDeclaration *syntaxCopy(Dsymbol *) override final;

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoStructDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoStructDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoClassDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoClassDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoInterfaceDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoInterfaceDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoPointerDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoPointerDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoArrayDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoArrayDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoStaticArrayDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoStaticArrayDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoAssociativeArrayDeclaration final : public TypeInfoDeclaration
{
public:
    Type* entry;

    static TypeInfoAssociativeArrayDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoEnumDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoEnumDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoFunctionDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoFunctionDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoDelegateDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoDelegateDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoTupleDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoTupleDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoConstDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoConstDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoInvariantDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoInvariantDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoSharedDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoSharedDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoWildDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoWildDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeInfoVectorDeclaration final : public TypeInfoDeclaration
{
public:
    static TypeInfoVectorDeclaration *create(Type *tinfo);

    void accept(Visitor *v) override { v->visit(this); }
};

/**************************************************************/

class ThisDeclaration final : public VarDeclaration
{
public:
    ThisDeclaration *syntaxCopy(Dsymbol *) override;
    void accept(Visitor *v) override { v->visit(this); }
};

enum class ILS : unsigned char
{
    ILSuninitialized,   // not computed yet
    ILSno,              // cannot inline
    ILSyes              // can inline
};

/**************************************************************/

enum class BUILTIN : unsigned char
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
    toPrecReal
};

Expression *eval_builtin(Loc loc, FuncDeclaration *fd, Expressions *arguments);
BUILTIN isBuiltin(FuncDeclaration *fd);

struct ContractInfo;

class FuncDeclaration : public Declaration
{
public:
    Statement *fbody;

    FuncDeclarations foverrides;        // functions this function overrides

private:
    ContractInfo *contracts;            // contract information

public:
    const char *mangleString;           // mangled symbol created from mangleExact()

    VarDeclaration *vresult;            // result variable for out contracts
    LabelDsymbol *returnLabel;          // where the return goes

    void *isTypeIsolatedCache;          // An AA on the D side to cache an expensive check result

    // used to prevent symbols in different
    // scopes from having the same name
    DsymbolTable *localsymtab;
    VarDeclaration *vthis;              // 'this' parameter (member and nested)
    VarDeclaration *v_arguments;        // '_arguments' parameter

    VarDeclaration *v_argptr;           // '_argptr' variable
    VarDeclarations *parameters;        // Array of VarDeclaration's for parameters
    DsymbolTable *labtab;               // statement label symbol table
    Dsymbol *overnext;                  // next in overload list
    FuncDeclaration *overnext0;         // next in overload list (only used during IFTI)
    Loc endloc;                         // location of closing curly bracket
    int vtblIndex;                      // for member functions, index into vtbl[]

    ILS inlineStatusStmt;
    ILS inlineStatusExp;
    PINLINE inlining;

    int inlineNest;                     // !=0 if nested inline

    // true if errors in semantic3 this function's frame ptr
    ForeachStatement *fes;              // if foreach body, this is the foreach
    BaseClass* interfaceVirtual;        // if virtual, but only appears in interface vtbl[]
    // if !=NULL, then this is the type
    // of the 'introducing' function
    // this one is overriding
    Type *tintro;
    StorageClass storage_class2;        // storage class for template onemember's

    // Things that should really go into Scope

    VarDeclaration *nrvo_var;           // variable to replace with shidden
    Symbol *shidden;                    // hidden pointer passed to function

    ReturnStatements *returns;

    GotoStatements *gotos;              // Gotos with forward references

    // set if this is a known, builtin function we can evaluate at compile time
    BUILTIN builtin;

    // set if someone took the address of this function
    int tookAddressOf;
    d_bool requiresClosure;               // this function needs a closure

    // local variables in this function which are referenced by nested functions
    VarDeclarations closureVars;

    /** Outer variables which are referenced by this nested function
     * (the inverse of closureVars)
     */
    VarDeclarations outerVars;

    // Sibling nested functions which called this one
    FuncDeclarations siblingCallers;

    FuncDeclarations *inlinedNestedCallees;

    AttributeViolation* safetyViolation;
    AttributeViolation* nogcViolation;
    AttributeViolation* pureViolation;
    AttributeViolation* nothrowViolation;

    // Formerly FUNCFLAGS
    uint32_t flags;
    bool purityInprocess() const;
    bool purityInprocess(bool v);
    bool safetyInprocess() const;
    bool safetyInprocess(bool v);
    bool nothrowInprocess() const;
    bool nothrowInprocess(bool v);
    bool nogcInprocess() const;
    bool nogcInprocess(bool v);
    bool saferD() const;
    bool saferD(bool v);
    bool scopeInprocess() const;
    bool scopeInprocess(bool v);
    bool inlineScanned() const;
    bool inlineScanned(bool v);
    bool hasCatches() const;
    bool hasCatches(bool v);
    bool skipCodegen() const;
    bool skipCodegen(bool v);
    bool printf() const;
    bool printf(bool v);
    bool scanf() const;
    bool scanf(bool v);
    bool noreturn() const;
    bool noreturn(bool v);
    bool isNRVO() const;
    bool isNRVO(bool v);
    bool isNaked() const;
    bool isNaked(bool v);
    bool isGenerated() const;
    bool isGenerated(bool v);
    bool isIntroducing() const;
    bool isIntroducing(bool v);
    bool hasSemantic3Errors() const;
    bool hasSemantic3Errors(bool v);
    bool hasNoEH() const;
    bool hasNoEH(bool v);
    bool inferRetType() const;
    bool inferRetType(bool v);
    bool hasDualContext() const;
    bool hasDualContext(bool v);
    bool hasAlwaysInlines() const;
    bool hasAlwaysInlines(bool v);
    bool isCrtCtor() const;
    bool isCrtCtor(bool v);
    bool isCrtDtor() const;
    bool isCrtDtor(bool v);
    bool dllImport() const;
    bool dllImport(bool v);
    bool dllExport() const;
    bool dllExport(bool v);
    bool hasReturnExp() const;
    bool hasReturnExp(bool v);
    bool hasInlineAsm() const;
    bool hasInlineAsm(bool v);
    bool hasMultipleReturnExp() const;
    bool hasMultipleReturnExp(bool v);

    // Data for a function declaration that is needed for the Objective-C
    // integration.
    ObjcFuncDeclaration objc;

    static FuncDeclaration *create(Loc loc, Loc endloc, Identifier *id, StorageClass storage_class, Type *type, bool noreturn = false);
    FuncDeclaration *syntaxCopy(Dsymbol *) override;
    Statements *frequires();
    Ensures *fensures();
    Statement *frequire();
    Statement *fensure();
    FuncDeclaration *fdrequire();
    FuncDeclaration *fdensure();
    Expressions *fdrequireParams();
    Expressions *fdensureParams();
    Statements *frequires(Statements *frs);
    Ensures *fensures(Statements *fes);
    Statement *frequire(Statement *fr);
    Statement *fensure(Statement *fe);
    FuncDeclaration *fdrequire(FuncDeclaration *fdr);
    FuncDeclaration *fdensure(FuncDeclaration *fde);
    Expressions *fdrequireParams(Expressions *fdrp);
    Expressions *fdensureParams(Expressions *fdep);
    bool equals(const RootObject * const o) const override final;

    bool overloadInsert(Dsymbol *s) override;
    bool inUnittest();
    LabelDsymbol *searchLabel(Identifier *ident, Loc loc);
    const char *toPrettyChars(bool QualifyTypes = false) override;
    const char *toFullSignature();  // for diagnostics, e.g. 'int foo(int x, int y) pure'
    bool isMain() const;
    bool isCMain() const;
    bool isWinMain() const;
    bool isDllMain() const;
    bool isExport() const override final;
    bool isImportedSymbol() const override final;
    bool isCodeseg() const override final;
    bool isOverloadable() const override final;
    bool isAbstract() override final;
    bool isSafe();
    bool isTrusted();

    virtual bool isNested() const;
    AggregateDeclaration *isThis() override;
    bool needThis() override final;
    bool isVirtualMethod();
    virtual bool isVirtual() const;
    bool isFinalFunc() const;
    virtual bool addPreInvariant();
    virtual bool addPostInvariant();
    const char *kind() const override;
    bool isUnique();
    bool needsClosure();
    bool hasNestedFrameRefs();
    ParameterList getParameterList();

    virtual FuncDeclaration *toAliasFunc() { return this; }
    void accept(Visitor *v) override { v->visit(this); }
};

class FuncAliasDeclaration final : public FuncDeclaration
{
public:
    FuncDeclaration *funcalias;
    d_bool hasOverloads;

    const char *kind() const override;

    FuncDeclaration *toAliasFunc() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class FuncLiteralDeclaration final : public FuncDeclaration
{
public:
    TOK tok;                       // TOKfunction or TOKdelegate
    Type *treq;                         // target of return type inference

    // backend
    d_bool deferToObj;

    FuncLiteralDeclaration *syntaxCopy(Dsymbol *) override;
    bool isNested() const override;
    AggregateDeclaration *isThis() override;
    bool isVirtual() const override;
    bool addPreInvariant() override;
    bool addPostInvariant() override;

    const char *kind() const override;
    const char *toPrettyChars(bool QualifyTypes = false) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class CtorDeclaration final : public FuncDeclaration
{
public:
    d_bool isCpCtor;
    d_bool isMoveCtor;
    CtorDeclaration *syntaxCopy(Dsymbol *) override;
    const char *kind() const override;
    bool isVirtual() const override;
    bool addPreInvariant() override;
    bool addPostInvariant() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class PostBlitDeclaration final : public FuncDeclaration
{
public:
    PostBlitDeclaration *syntaxCopy(Dsymbol *) override;
    bool isVirtual() const override;
    bool addPreInvariant() override;
    bool addPostInvariant() override;
    bool overloadInsert(Dsymbol *s) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class DtorDeclaration final : public FuncDeclaration
{
public:
    DtorDeclaration *syntaxCopy(Dsymbol *) override;
    const char *kind() const override;
    bool isVirtual() const override;
    bool addPreInvariant() override;
    bool addPostInvariant() override;
    bool overloadInsert(Dsymbol *s) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class StaticCtorDeclaration : public FuncDeclaration
{
public:
    StaticCtorDeclaration *syntaxCopy(Dsymbol *) override;
    AggregateDeclaration *isThis() override final;
    bool isVirtual() const override final;
    bool addPreInvariant() override final;
    bool addPostInvariant() override final;

    void accept(Visitor *v) override { v->visit(this); }
};

class SharedStaticCtorDeclaration final : public StaticCtorDeclaration
{
public:
    bool standalone;
    SharedStaticCtorDeclaration *syntaxCopy(Dsymbol *) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class StaticDtorDeclaration : public FuncDeclaration
{
public:
    VarDeclaration *vgate;      // 'gate' variable

    StaticDtorDeclaration *syntaxCopy(Dsymbol *) override;
    AggregateDeclaration *isThis() override final;
    bool isVirtual() const override final;
    bool addPreInvariant() override final;
    bool addPostInvariant() override final;

    void accept(Visitor *v) override { v->visit(this); }
};

class SharedStaticDtorDeclaration final : public StaticDtorDeclaration
{
public:
    SharedStaticDtorDeclaration *syntaxCopy(Dsymbol *) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class InvariantDeclaration final : public FuncDeclaration
{
public:
    InvariantDeclaration *syntaxCopy(Dsymbol *) override;
    bool isVirtual() const override;
    bool addPreInvariant() override;
    bool addPostInvariant() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class UnitTestDeclaration final : public FuncDeclaration
{
public:
    char *codedoc; /** For documented unittest. */

    // toObjFile() these nested functions after this one
    FuncDeclarations deferredNested;

    UnitTestDeclaration *syntaxCopy(Dsymbol *) override;
    AggregateDeclaration *isThis() override;
    bool isVirtual() const override;
    bool addPreInvariant() override;
    bool addPostInvariant() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class NewDeclaration final : public FuncDeclaration
{
public:
    NewDeclaration *syntaxCopy(Dsymbol *) override;
    const char *kind() const override;
    bool isVirtual() const override;
    bool addPreInvariant() override;
    bool addPostInvariant() override;

    void accept(Visitor *v) override { v->visit(this); }
};
