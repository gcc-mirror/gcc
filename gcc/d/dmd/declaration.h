
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2022 by The D Language Foundation, All Rights Reserved
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

    //                            0x10000ULL
    #define STCtemplateparameter  0x20000ULL    /// template parameter
    #define STCref                0x40000ULL    /// `ref`
    #define STCscope              0x80000ULL    /// `scope`

    #define STCmaybescope         0x100000ULL    /// parameter might be `scope`
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
    #define STCtls                0x80000000ULL    /// thread local

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

void ObjectNotFound(Identifier *id);

/**************************************************************/

class Declaration : public Dsymbol
{
public:
    Type *type;
    Type *originalType;         // before semantic analysis
    StorageClass storage_class;
    Visibility visibility;
    LINK linkage;
    short inuse;                // used to detect cycles
    uint8_t adFlags;
    DString mangleOverride;     // overridden symbol with pragma(mangle, "...")

    const char *kind() const;
    d_uns64 size(const Loc &loc);

    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = SearchLocalsOnly);

    bool isStatic() const { return (storage_class & STCstatic) != 0; }
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
    bool isSynchronized() const { return (storage_class & STCsynchronized) != 0; }
    bool isParameter() const    { return (storage_class & STCparameter) != 0; }
    bool isDeprecated() const   { return (storage_class & STCdeprecated) != 0; }
    bool isOverride() const     { return (storage_class & STCoverride) != 0; }
    bool isResult() const       { return (storage_class & STCresult) != 0; }
    bool isField() const        { return (storage_class & STCfield) != 0; }

    bool isIn()  const  { return (storage_class & STCin) != 0; }
    bool isOut() const  { return (storage_class & STCout) != 0; }
    bool isRef() const  { return (storage_class & STCref) != 0; }
    bool isReference() const { return (storage_class & (STCref | STCout)) != 0; }

    bool isFuture() const { return (storage_class & STCfuture) != 0; }

    Visibility visible();

    Declaration *isDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

/**************************************************************/

class TupleDeclaration : public Declaration
{
public:
    Objects *objects;
    bool isexp;                 // true: expression tuple

    TypeTuple *tupletype;       // !=NULL if this is a type tuple

    TupleDeclaration *syntaxCopy(Dsymbol *);
    const char *kind() const;
    Type *getType();
    Dsymbol *toAlias2();
    bool needThis();

    TupleDeclaration *isTupleDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

/**************************************************************/

class AliasDeclaration : public Declaration
{
public:
    Dsymbol *aliassym;
    Dsymbol *overnext;          // next in overload list
    Dsymbol *_import;           // !=NULL if unresolved internal alias for selective import

    static AliasDeclaration *create(const Loc &loc, Identifier *id, Type *type);
    AliasDeclaration *syntaxCopy(Dsymbol *);
    bool overloadInsert(Dsymbol *s);
    const char *kind() const;
    Type *getType();
    Dsymbol *toAlias();
    Dsymbol *toAlias2();
    bool isOverloadable() const;

    AliasDeclaration *isAliasDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

/**************************************************************/

class OverDeclaration : public Declaration
{
public:
    Dsymbol *overnext;          // next in overload list
    Dsymbol *aliassym;

    const char *kind() const;
    bool equals(const RootObject *o) const;
    bool overloadInsert(Dsymbol *s);

    Dsymbol *toAlias();
    Dsymbol *isUnique();
    bool isOverloadable() const;

    OverDeclaration *isOverDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

/**************************************************************/

class VarDeclaration : public Declaration
{
public:
    Initializer *_init;
    FuncDeclarations nestedrefs; // referenced by these lexically nested functions
    Dsymbol *aliassym;          // if redone as alias to another symbol
    VarDeclaration *lastVar;    // Linked list of variables for goto-skips-init detection
    Expression *edtor;          // if !=NULL, does the destruction of the variable
    IntRange *range;            // if !NULL, the variable is known to be within the range
    VarDeclarations *maybes;    // STCmaybescope variables that are assigned to this STCmaybescope variable

    unsigned endlinnum;         // line number of end of scope that this var lives in
    unsigned offset;
    unsigned sequenceNumber;     // order the variables are declared
    structalign_t alignment;

    // When interpreting, these point to the value (NULL if value not determinable)
    // The index of this variable on the CTFE stack, ~0u if not allocated
    unsigned ctfeAdrOnStack;

    bool isargptr;              // if parameter that _argptr points to
    bool ctorinit;              // it has been initialized in a ctor
    bool iscatchvar;            // this is the exception object variable in catch() clause
    bool isowner;               // this is an Owner, despite it being `scope`
    bool setInCtorOnly;         // field can only be set in a constructor, as it is const or immutable
    bool onstack;               // it is a class that was allocated on the stack
    bool mynew;                 // it is a class new'd with custom operator new
    char canassign;             // it can be assigned to
    bool overlapped;            // if it is a field and has overlapping
    bool overlapUnsafe;         // if it is an overlapping field and the overlaps are unsafe
    bool doNotInferScope;       // do not infer 'scope' for this variable
    bool doNotInferReturn;      // do not infer 'return' for this variable
    unsigned char isdataseg;    // private data for isDataseg
    bool isArgDtorVar;          // temporary created to handle scope destruction of a function argument

public:
    static VarDeclaration *create(const Loc &loc, Type *t, Identifier *id, Initializer *init, StorageClass storage_class = STCundefined);
    VarDeclaration *syntaxCopy(Dsymbol *);
    void setFieldOffset(AggregateDeclaration *ad, FieldState& fieldState, bool isunion);
    const char *kind() const;
    AggregateDeclaration *isThis();
    bool needThis();
    bool isExport() const;
    bool isImportedSymbol() const;
    bool isCtorinit() const;
    bool isDataseg();
    bool isThreadlocal();
    bool isCTFE();
    bool isOverlappedWith(VarDeclaration *v);
    bool hasPointers();
    bool canTakeAddressOf();
    bool needsScopeDtor();
    bool enclosesLifetimeOf(VarDeclaration *v) const;
    void checkCtorConstInit();
    Dsymbol *toAlias();
    // Eliminate need for dynamic_cast
    VarDeclaration *isVarDeclaration() { return (VarDeclaration *)this; }
    void accept(Visitor *v) { v->visit(this); }
};

/**************************************************************/

class BitFieldDeclaration : public VarDeclaration
{
public:
    Expression *width;

    unsigned fieldWidth;
    unsigned bitOffset;

    BitFieldDeclaration *syntaxCopy(Dsymbol*);
    BitFieldDeclaration *isBitFieldDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

/**************************************************************/

// This is a shell around a back end symbol

class SymbolDeclaration : public Declaration
{
public:
    AggregateDeclaration *dsym;

    // Eliminate need for dynamic_cast
    SymbolDeclaration *isSymbolDeclaration() { return (SymbolDeclaration *)this; }
    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoDeclaration : public VarDeclaration
{
public:
    Type *tinfo;

    static TypeInfoDeclaration *create(Type *tinfo);
    TypeInfoDeclaration *syntaxCopy(Dsymbol *);
    const char *toChars() const;

    TypeInfoDeclaration *isTypeInfoDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoStructDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoStructDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoClassDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoClassDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoInterfaceDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoInterfaceDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoPointerDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoPointerDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoArrayDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoArrayDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoStaticArrayDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoStaticArrayDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoAssociativeArrayDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoAssociativeArrayDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoEnumDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoEnumDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoFunctionDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoFunctionDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoDelegateDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoDelegateDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoTupleDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoTupleDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoConstDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoConstDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoInvariantDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoInvariantDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoSharedDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoSharedDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoWildDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoWildDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeInfoVectorDeclaration : public TypeInfoDeclaration
{
public:
    static TypeInfoVectorDeclaration *create(Type *tinfo);

    void accept(Visitor *v) { v->visit(this); }
};

/**************************************************************/

class ThisDeclaration : public VarDeclaration
{
public:
    ThisDeclaration *syntaxCopy(Dsymbol *);
    ThisDeclaration *isThisDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
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

Expression *eval_builtin(const Loc &loc, FuncDeclaration *fd, Expressions *arguments);
BUILTIN isBuiltin(FuncDeclaration *fd);

class FuncDeclaration : public Declaration
{
public:
    Statements *frequires;              // in contracts
    Ensures *fensures;                  // out contracts
    Statement *frequire;                // lowered in contract
    Statement *fensure;                 // lowered out contract
    Statement *fbody;

    FuncDeclarations foverrides;        // functions this function overrides
    FuncDeclaration *fdrequire;         // function that does the in contract
    FuncDeclaration *fdensure;          // function that does the out contract

    Expressions *fdrequireParams;       // argument list for __require
    Expressions *fdensureParams;        // argument list for __ensure

    const char *mangleString;           // mangled symbol created from mangleExact()

    VarDeclaration *vresult;            // result variable for out contracts
    LabelDsymbol *returnLabel;          // where the return goes

    void *isTypeIsolatedCache;          // An AA on the D side to cache an expensive check result

    // used to prevent symbols in different
    // scopes from having the same name
    DsymbolTable *localsymtab;
    VarDeclaration *vthis;              // 'this' parameter (member and nested)
    bool isThis2;                       // has a dual-context 'this' parameter
    VarDeclaration *v_arguments;        // '_arguments' parameter

    VarDeclaration *v_argptr;           // '_argptr' variable
    VarDeclarations *parameters;        // Array of VarDeclaration's for parameters
    DsymbolTable *labtab;               // statement label symbol table
    Dsymbol *overnext;                  // next in overload list
    FuncDeclaration *overnext0;         // next in overload list (only used during IFTI)
    Loc endloc;                         // location of closing curly bracket
    int vtblIndex;                      // for member functions, index into vtbl[]
    bool naked;                         // true if naked
    bool generated;                     // true if function was generated by the compiler rather than
                                        // supplied by the user
    bool hasAlwaysInlines;              // contains references to functions that must be inlined
    unsigned char isCrtCtorDtor;        // has attribute pragma(crt_constructor(1)/crt_destructor(2))
                                        // not set before the glue layer
    ILS inlineStatusStmt;
    ILS inlineStatusExp;
    PINLINE inlining;

    int inlineNest;                     // !=0 if nested inline
    bool eh_none;                       /// true if no exception unwinding is needed

    // true if errors in semantic3 this function's frame ptr
    bool semantic3Errors;
    ForeachStatement *fes;              // if foreach body, this is the foreach
    BaseClass* interfaceVirtual;        // if virtual, but only appears in interface vtbl[]
    bool introducing;                   // true if 'introducing' function
    // if !=NULL, then this is the type
    // of the 'introducing' function
    // this one is overriding
    Type *tintro;
    bool inferRetType;                  // true if return type is to be inferred
    StorageClass storage_class2;        // storage class for template onemember's

    // Things that should really go into Scope

    // 1 if there's a return exp; statement
    // 2 if there's a throw statement
    // 4 if there's an assert(0)
    // 8 if there's inline asm
    // 16 if there are multiple return statements
    int hasReturnExp;

    // Support for NRVO (named return value optimization)
    bool nrvo_can;                      // true means we can do it
    VarDeclaration *nrvo_var;           // variable to replace with shidden
    Symbol *shidden;                    // hidden pointer passed to function

    ReturnStatements *returns;

    GotoStatements *gotos;              // Gotos with forward references

    // set if this is a known, builtin function we can evaluate at compile time
    BUILTIN builtin;

    // set if someone took the address of this function
    int tookAddressOf;
    bool requiresClosure;               // this function needs a closure

    // local variables in this function which are referenced by nested functions
    VarDeclarations closureVars;

    /** Outer variables which are referenced by this nested function
     * (the inverse of closureVars)
     */
    VarDeclarations outerVars;

    // Sibling nested functions which called this one
    FuncDeclarations siblingCallers;

    FuncDeclarations *inlinedNestedCallees;

    unsigned flags;                     // FUNCFLAGxxxxx

    // Data for a function declaration that is needed for the Objective-C
    // integration.
    ObjcFuncDeclaration objc;

    static FuncDeclaration *create(const Loc &loc, const Loc &endloc, Identifier *id, StorageClass storage_class, Type *type, bool noreturn = false);
    FuncDeclaration *syntaxCopy(Dsymbol *);
    bool functionSemantic();
    bool functionSemantic3();
    bool equals(const RootObject *o) const;

    int overrides(FuncDeclaration *fd);
    int findVtblIndex(Dsymbols *vtbl, int dim);
    BaseClass *overrideInterface();
    bool overloadInsert(Dsymbol *s);
    bool inUnittest();
    MATCH leastAsSpecialized(FuncDeclaration *g);
    LabelDsymbol *searchLabel(Identifier *ident, const Loc &loc);
    int getLevel(FuncDeclaration *fd, int intypeof); // lexical nesting level difference
    int getLevelAndCheck(const Loc &loc, Scope *sc, FuncDeclaration *fd);
    const char *toPrettyChars(bool QualifyTypes = false);
    const char *toFullSignature();  // for diagnostics, e.g. 'int foo(int x, int y) pure'
    bool isMain() const;
    bool isCMain() const;
    bool isWinMain() const;
    bool isDllMain() const;
    bool isExport() const;
    bool isImportedSymbol() const;
    bool isCodeseg() const;
    bool isOverloadable() const;
    bool isAbstract();
    PURE isPure();
    PURE isPureBypassingInference();
    bool isSafe();
    bool isSafeBypassingInference();
    bool isTrusted();

    bool isNogc();
    bool isNogcBypassingInference();

    virtual bool isNested() const;
    AggregateDeclaration *isThis();
    bool needThis();
    bool isVirtualMethod();
    virtual bool isVirtual() const;
    bool isFinalFunc() const;
    virtual bool addPreInvariant();
    virtual bool addPostInvariant();
    const char *kind() const;
    bool isUnique();
    bool needsClosure();
    bool hasNestedFrameRefs();
    ParameterList getParameterList();

    static FuncDeclaration *genCfunc(Parameters *args, Type *treturn, const char *name, StorageClass stc=0);
    static FuncDeclaration *genCfunc(Parameters *args, Type *treturn, Identifier *id, StorageClass stc=0);

    bool checkNRVO();

    FuncDeclaration *isFuncDeclaration() { return this; }

    virtual FuncDeclaration *toAliasFunc() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class FuncAliasDeclaration : public FuncDeclaration
{
public:
    FuncDeclaration *funcalias;
    bool hasOverloads;

    FuncAliasDeclaration *isFuncAliasDeclaration() { return this; }
    const char *kind() const;

    FuncDeclaration *toAliasFunc();
    void accept(Visitor *v) { v->visit(this); }
};

class FuncLiteralDeclaration : public FuncDeclaration
{
public:
    TOK tok;                       // TOKfunction or TOKdelegate
    Type *treq;                         // target of return type inference

    // backend
    bool deferToObj;

    FuncLiteralDeclaration *syntaxCopy(Dsymbol *);
    bool isNested() const;
    AggregateDeclaration *isThis();
    bool isVirtual() const;
    bool addPreInvariant();
    bool addPostInvariant();

    void modifyReturns(Scope *sc, Type *tret);

    FuncLiteralDeclaration *isFuncLiteralDeclaration() { return this; }
    const char *kind() const;
    const char *toPrettyChars(bool QualifyTypes = false);
    void accept(Visitor *v) { v->visit(this); }
};

class CtorDeclaration : public FuncDeclaration
{
public:
    bool isCpCtor;
    CtorDeclaration *syntaxCopy(Dsymbol *);
    const char *kind() const;
    const char *toChars() const;
    bool isVirtual() const;
    bool addPreInvariant();
    bool addPostInvariant();

    CtorDeclaration *isCtorDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class PostBlitDeclaration : public FuncDeclaration
{
public:
    PostBlitDeclaration *syntaxCopy(Dsymbol *);
    bool isVirtual() const;
    bool addPreInvariant();
    bool addPostInvariant();
    bool overloadInsert(Dsymbol *s);

    PostBlitDeclaration *isPostBlitDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class DtorDeclaration : public FuncDeclaration
{
public:
    DtorDeclaration *syntaxCopy(Dsymbol *);
    const char *kind() const;
    const char *toChars() const;
    bool isVirtual() const;
    bool addPreInvariant();
    bool addPostInvariant();
    bool overloadInsert(Dsymbol *s);

    DtorDeclaration *isDtorDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class StaticCtorDeclaration : public FuncDeclaration
{
public:
    StaticCtorDeclaration *syntaxCopy(Dsymbol *);
    AggregateDeclaration *isThis();
    bool isVirtual() const;
    bool addPreInvariant();
    bool addPostInvariant();
    bool hasStaticCtorOrDtor();

    StaticCtorDeclaration *isStaticCtorDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class SharedStaticCtorDeclaration : public StaticCtorDeclaration
{
public:
    SharedStaticCtorDeclaration *syntaxCopy(Dsymbol *);

    SharedStaticCtorDeclaration *isSharedStaticCtorDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class StaticDtorDeclaration : public FuncDeclaration
{
public:
    VarDeclaration *vgate;      // 'gate' variable

    StaticDtorDeclaration *syntaxCopy(Dsymbol *);
    AggregateDeclaration *isThis();
    bool isVirtual() const;
    bool hasStaticCtorOrDtor();
    bool addPreInvariant();
    bool addPostInvariant();

    StaticDtorDeclaration *isStaticDtorDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class SharedStaticDtorDeclaration : public StaticDtorDeclaration
{
public:
    SharedStaticDtorDeclaration *syntaxCopy(Dsymbol *);

    SharedStaticDtorDeclaration *isSharedStaticDtorDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class InvariantDeclaration : public FuncDeclaration
{
public:
    InvariantDeclaration *syntaxCopy(Dsymbol *);
    bool isVirtual() const;
    bool addPreInvariant();
    bool addPostInvariant();

    InvariantDeclaration *isInvariantDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class UnitTestDeclaration : public FuncDeclaration
{
public:
    char *codedoc; /** For documented unittest. */

    // toObjFile() these nested functions after this one
    FuncDeclarations deferredNested;

    UnitTestDeclaration *syntaxCopy(Dsymbol *);
    AggregateDeclaration *isThis();
    bool isVirtual() const;
    bool addPreInvariant();
    bool addPostInvariant();

    UnitTestDeclaration *isUnitTestDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class NewDeclaration : public FuncDeclaration
{
public:
    NewDeclaration *syntaxCopy(Dsymbol *);
    const char *kind() const;
    bool isVirtual() const;
    bool addPreInvariant();
    bool addPostInvariant();

    NewDeclaration *isNewDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};
