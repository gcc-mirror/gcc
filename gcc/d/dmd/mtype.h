
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/mtype.h
 */

#pragma once

#include "root/root.h"
#include "root/stringtable.h"
#include "root/dcompat.h" // for d_size_t

#include "arraytypes.h"
#include "ast_node.h"
#include "expression.h"
#include "visitor.h"

struct Scope;
class Identifier;
class Expression;
class StructDeclaration;
class ClassDeclaration;
class EnumDeclaration;
class TypeInfoDeclaration;
class Dsymbol;
class TemplateInstance;
class TemplateDeclaration;
enum LINK;

class TypeBasic;
class Parameter;

// Back end
#ifdef IN_GCC
typedef union tree_node type;
#else
typedef struct TYPE type;
#endif

void semanticTypeInfo(Scope *sc, Type *t);
MATCH deduceType(RootObject *o, Scope *sc, Type *tparam, TemplateParameters *parameters, Objects *dedtypes, unsigned *wm = NULL, size_t inferStart = 0);
StorageClass ModToStc(unsigned mod);

enum ENUMTY
{
    Tarray,             // slice array, aka T[]
    Tsarray,            // static array, aka T[dimension]
    Taarray,            // associative array, aka T[type]
    Tpointer,
    Treference,
    Tfunction,
    Tident,
    Tclass,
    Tstruct,
    Tenum,

    Tdelegate,
    Tnone,
    Tvoid,
    Tint8,
    Tuns8,
    Tint16,
    Tuns16,
    Tint32,
    Tuns32,
    Tint64,

    Tuns64,
    Tfloat32,
    Tfloat64,
    Tfloat80,
    Timaginary32,
    Timaginary64,
    Timaginary80,
    Tcomplex32,
    Tcomplex64,
    Tcomplex80,

    Tbool,
    Tchar,
    Twchar,
    Tdchar,
    Terror,
    Tinstance,
    Ttypeof,
    Ttuple,
    Tslice,
    Treturn,

    Tnull,
    Tvector,
    Tint128,
    Tuns128,
    Ttraits,
    TMAX
};
typedef unsigned char TY;       // ENUMTY

#define SIZE_INVALID (~(d_uns64)0)   // error return from size() functions


/**
 * type modifiers
 * pick this order of numbers so switch statements work better
 */
enum MODFlags
{
    MODconst     = 1, // type is const
    MODimmutable = 4, // type is immutable
    MODshared    = 2, // type is shared
    MODwild      = 8, // type is wild
    MODwildconst = (MODwild | MODconst), // type is wild const
    MODmutable   = 0x10       // type is mutable (only used in wildcard matching)
};
typedef unsigned char MOD;

// These tables are for implicit conversion of binary ops;
// the indices are the type of operand one, followed by operand two.
extern unsigned char impcnvResult[TMAX][TMAX];
extern unsigned char impcnvType1[TMAX][TMAX];
extern unsigned char impcnvType2[TMAX][TMAX];

// If !=0, give warning on implicit conversion
extern unsigned char impcnvWarn[TMAX][TMAX];

enum VarArg
{
    VARARGnone     = 0,  /// fixed number of arguments
    VARARGvariadic = 1,  /// T t, ...)  can be C-style (core.stdc.stdarg) or D-style (core.vararg)
    VARARGtypesafe = 2   /// T t ...) typesafe https://dlang.org/spec/function.html#typesafe_variadic_functions
                         ///   or https://dlang.org/spec/function.html#typesafe_variadic_functions
};

class Type : public ASTNode
{
public:
    TY ty;
    MOD mod;  // modifiers MODxxxx
    char *deco;

    /* These are cached values that are lazily evaluated by constOf(), immutableOf(), etc.
     * They should not be referenced by anybody but mtype.c.
     * They can be NULL if not lazily evaluated yet.
     * Note that there is no "shared immutable", because that is just immutable
     * Naked == no MOD bits
     */

    Type *cto;          // MODconst                 ? naked version of this type : const version
    Type *ito;          // MODimmutable             ? naked version of this type : immutable version
    Type *sto;          // MODshared                ? naked version of this type : shared mutable version
    Type *scto;         // MODshared | MODconst     ? naked version of this type : shared const version
    Type *wto;          // MODwild                  ? naked version of this type : wild version
    Type *wcto;         // MODwildconst             ? naked version of this type : wild const version
    Type *swto;         // MODshared | MODwild      ? naked version of this type : shared wild version
    Type *swcto;        // MODshared | MODwildconst ? naked version of this type : shared wild const version

    Type *pto;          // merged pointer to this type
    Type *rto;          // reference to this type
    Type *arrayof;      // array of this type
    TypeInfoDeclaration *vtinfo;        // TypeInfo object for this Type

    type *ctype;        // for back end

    static Type *tvoid;
    static Type *tint8;
    static Type *tuns8;
    static Type *tint16;
    static Type *tuns16;
    static Type *tint32;
    static Type *tuns32;
    static Type *tint64;
    static Type *tuns64;
    static Type *tint128;
    static Type *tuns128;
    static Type *tfloat32;
    static Type *tfloat64;
    static Type *tfloat80;

    static Type *timaginary32;
    static Type *timaginary64;
    static Type *timaginary80;

    static Type *tcomplex32;
    static Type *tcomplex64;
    static Type *tcomplex80;

    static Type *tbool;
    static Type *tchar;
    static Type *twchar;
    static Type *tdchar;

    // Some special types
    static Type *tshiftcnt;
    static Type *tvoidptr;              // void*
    static Type *tstring;               // immutable(char)[]
    static Type *twstring;              // immutable(wchar)[]
    static Type *tdstring;              // immutable(dchar)[]
    static Type *terror;                // for error recovery
    static Type *tnull;                 // for null type

    static Type *tsize_t;               // matches size_t alias
    static Type *tptrdiff_t;            // matches ptrdiff_t alias
    static Type *thash_t;               // matches hash_t alias

    static ClassDeclaration *dtypeinfo;
    static ClassDeclaration *typeinfoclass;
    static ClassDeclaration *typeinfointerface;
    static ClassDeclaration *typeinfostruct;
    static ClassDeclaration *typeinfopointer;
    static ClassDeclaration *typeinfoarray;
    static ClassDeclaration *typeinfostaticarray;
    static ClassDeclaration *typeinfoassociativearray;
    static ClassDeclaration *typeinfovector;
    static ClassDeclaration *typeinfoenum;
    static ClassDeclaration *typeinfofunction;
    static ClassDeclaration *typeinfodelegate;
    static ClassDeclaration *typeinfotypelist;
    static ClassDeclaration *typeinfoconst;
    static ClassDeclaration *typeinfoinvariant;
    static ClassDeclaration *typeinfoshared;
    static ClassDeclaration *typeinfowild;

    static TemplateDeclaration *rtinfo;

    static Type *basic[TMAX];
    static unsigned char sizeTy[TMAX];
    static StringTable stringtable;

    Type(TY ty);
    virtual const char *kind();
    Type *copy();
    virtual Type *syntaxCopy();
    bool equals(RootObject *o);
    bool equivalent(Type *t);
    // kludge for template.isType()
    int dyncast() const { return DYNCAST_TYPE; }
    int covariant(Type *t, StorageClass *pstc = NULL, bool fix17349 = true);
    const char *toChars();
    char *toPrettyChars(bool QualifyTypes = false);
    static void _init();

    d_uns64 size();
    virtual d_uns64 size(Loc loc);
    virtual unsigned alignsize();
    virtual Type *semantic(Loc loc, Scope *sc);
    Type *trySemantic(Loc loc, Scope *sc);
    Type *merge();
    Type *merge2();
    void modToBuffer(OutBuffer *buf);
    char *modToChars();

    /** For each active modifier (MODconst, MODimmutable, etc) call fp with a
    void* for the work param and a string representation of the attribute. */
    int modifiersApply(void *param, int (*fp)(void *, const char *));

    virtual bool isintegral();
    virtual bool isfloating();   // real, imaginary, or complex
    virtual bool isreal();
    virtual bool isimaginary();
    virtual bool iscomplex();
    virtual bool isscalar();
    virtual bool isunsigned();
    virtual bool isscope();
    virtual bool isString();
    virtual bool isAssignable();
    virtual bool isBoolean();
    virtual void checkDeprecated(Loc loc, Scope *sc);
    bool isConst() const       { return (mod & MODconst) != 0; }
    bool isImmutable() const   { return (mod & MODimmutable) != 0; }
    bool isMutable() const     { return (mod & (MODconst | MODimmutable | MODwild)) == 0; }
    bool isShared() const      { return (mod & MODshared) != 0; }
    bool isSharedConst() const { return (mod & (MODshared | MODconst)) == (MODshared | MODconst); }
    bool isWild() const        { return (mod & MODwild) != 0; }
    bool isWildConst() const   { return (mod & MODwildconst) == MODwildconst; }
    bool isSharedWild() const  { return (mod & (MODshared | MODwild)) == (MODshared | MODwild); }
    bool isNaked() const       { return mod == 0; }
    Type *nullAttributes();
    Type *constOf();
    Type *immutableOf();
    Type *mutableOf();
    Type *sharedOf();
    Type *sharedConstOf();
    Type *unSharedOf();
    Type *wildOf();
    Type *wildConstOf();
    Type *sharedWildOf();
    Type *sharedWildConstOf();
    void fixTo(Type *t);
    void check();
    Type *addSTC(StorageClass stc);
    Type *castMod(MOD mod);
    Type *addMod(MOD mod);
    virtual Type *addStorageClass(StorageClass stc);
    Type *pointerTo();
    Type *referenceTo();
    Type *arrayOf();
    Type *sarrayOf(dinteger_t dim);
    Type *aliasthisOf();
    bool checkAliasThisRec();
    virtual Type *makeConst();
    virtual Type *makeImmutable();
    virtual Type *makeShared();
    virtual Type *makeSharedConst();
    virtual Type *makeWild();
    virtual Type *makeWildConst();
    virtual Type *makeSharedWild();
    virtual Type *makeSharedWildConst();
    virtual Type *makeMutable();
    virtual Dsymbol *toDsymbol(Scope *sc);
    virtual Type *toBasetype();
    virtual bool isBaseOf(Type *t, int *poffset);
    virtual MATCH implicitConvTo(Type *to);
    virtual MATCH constConv(Type *to);
    virtual unsigned char deduceWild(Type *t, bool isRef);
    virtual Type *substWildTo(unsigned mod);

    Type *unqualify(unsigned m);

    virtual Type *toHeadMutable();
    virtual ClassDeclaration *isClassHandle();
    virtual Expression *getProperty(Loc loc, Identifier *ident, int flag);
    virtual Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    virtual structalign_t alignment();
    Expression *noMember(Scope *sc, Expression *e, Identifier *ident, int flag);
    virtual Expression *defaultInit(Loc loc = Loc());
    virtual Expression *defaultInitLiteral(Loc loc);
    virtual bool isZeroInit(Loc loc = Loc());                // if initializer is 0
    Identifier *getTypeInfoIdent();
    virtual void resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid = false);
    void resolveExp(Expression *e, Type **pt, Expression **pe, Dsymbol **ps);
    virtual int hasWild() const;
    virtual bool hasPointers();
    virtual bool hasVoidInitPointers();
    virtual Type *nextOf();
    Type *baseElemOf();
    uinteger_t sizemask();
    unsigned numberOfElems(const Loc &loc);
    virtual bool needsDestruction();
    virtual bool needsNested();
    void checkComplexTransition(Loc loc);
    TypeFunction *toTypeFunction();

    static void error(Loc loc, const char *format, ...);
    static void warning(Loc loc, const char *format, ...);

    // For eliminating dynamic_cast
    virtual TypeBasic *isTypeBasic();
    TypeError *isTypeError();
    TypeVector *isTypeVector();
    TypeSArray *isTypeSArray();
    TypeDArray *isTypeDArray();
    TypeAArray *isTypeAArray();
    TypePointer *isTypePointer();
    TypeReference *isTypeReference();
    TypeFunction *isTypeFunction();
    TypeDelegate *isTypeDelegate();
    TypeIdentifier *isTypeIdentifier();
    TypeInstance *isTypeInstance();
    TypeTypeof *isTypeTypeof();
    TypeReturn *isTypeReturn();
    TypeStruct *isTypeStruct();
    TypeEnum *isTypeEnum();
    TypeClass *isTypeClass();
    TypeTuple *isTypeTuple();
    TypeSlice *isTypeSlice();
    TypeNull *isTypeNull();
    TypeTraits *isTypeTraits();

    void accept(Visitor *v) { v->visit(this); }
};

class TypeError : public Type
{
public:
    TypeError();
    Type *syntaxCopy();

    d_uns64 size(Loc loc);
    Expression *getProperty(Loc loc, Identifier *ident, int flag);
    Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    Expression *defaultInit(Loc loc);
    Expression *defaultInitLiteral(Loc loc);
    void accept(Visitor *v) { v->visit(this); }
};

class TypeNext : public Type
{
public:
    Type *next;

    TypeNext(TY ty, Type *next);
    void checkDeprecated(Loc loc, Scope *sc);
    int hasWild() const;
    Type *nextOf();
    Type *makeConst();
    Type *makeImmutable();
    Type *makeShared();
    Type *makeSharedConst();
    Type *makeWild();
    Type *makeWildConst();
    Type *makeSharedWild();
    Type *makeSharedWildConst();
    Type *makeMutable();
    MATCH constConv(Type *to);
    unsigned char deduceWild(Type *t, bool isRef);
    void transitive();
    void accept(Visitor *v) { v->visit(this); }
};

class TypeBasic : public Type
{
public:
    const char *dstring;
    unsigned flags;

    TypeBasic(TY ty);
    const char *kind();
    Type *syntaxCopy();
    d_uns64 size(Loc loc) /*const*/;
    unsigned alignsize();
    Expression *getProperty(Loc loc, Identifier *ident, int flag);
    Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    bool isintegral();
    bool isfloating() /*const*/;
    bool isreal() /*const*/;
    bool isimaginary() /*const*/;
    bool iscomplex() /*const*/;
    bool isscalar() /*const*/;
    bool isunsigned() /*const*/;
    MATCH implicitConvTo(Type *to);
    Expression *defaultInit(Loc loc);
    bool isZeroInit(Loc loc) /*const*/;

    // For eliminating dynamic_cast
    TypeBasic *isTypeBasic();
    void accept(Visitor *v) { v->visit(this); }
};

class TypeVector : public Type
{
public:
    Type *basetype;

    TypeVector(Type *basetype);
    static TypeVector *create(Type *basetype);
    const char *kind();
    Type *syntaxCopy();
    Type *semantic(Loc loc, Scope *sc);
    d_uns64 size(Loc loc);
    unsigned alignsize();
    Expression *getProperty(Loc loc, Identifier *ident, int flag);
    Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    bool isintegral();
    bool isfloating();
    bool isscalar();
    bool isunsigned();
    bool isBoolean() /*const*/;
    MATCH implicitConvTo(Type *to);
    Expression *defaultInit(Loc loc);
    Expression *defaultInitLiteral(Loc loc);
    TypeBasic *elementType();
    bool isZeroInit(Loc loc);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeArray : public TypeNext
{
public:
    TypeArray(TY ty, Type *next);
    Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    void accept(Visitor *v) { v->visit(this); }
};

// Static array, one with a fixed dimension
class TypeSArray : public TypeArray
{
public:
    Expression *dim;

    TypeSArray(Type *t, Expression *dim);
    const char *kind();
    Type *syntaxCopy();
    d_uns64 size(Loc loc);
    unsigned alignsize();
    Type *semantic(Loc loc, Scope *sc);
    void resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid = false);
    Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    bool isString();
    bool isZeroInit(Loc loc);
    structalign_t alignment();
    MATCH constConv(Type *to);
    MATCH implicitConvTo(Type *to);
    Expression *defaultInit(Loc loc);
    Expression *defaultInitLiteral(Loc loc);
    bool hasPointers();
    bool needsDestruction();
    bool needsNested();

    void accept(Visitor *v) { v->visit(this); }
};

// Dynamic array, no dimension
class TypeDArray : public TypeArray
{
public:
    TypeDArray(Type *t);
    const char *kind();
    Type *syntaxCopy();
    d_uns64 size(Loc loc) /*const*/;
    unsigned alignsize() /*const*/;
    Type *semantic(Loc loc, Scope *sc);
    void resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid = false);
    Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    bool isString();
    bool isZeroInit(Loc loc) /*const*/;
    bool isBoolean() /*const*/;
    MATCH implicitConvTo(Type *to);
    Expression *defaultInit(Loc loc);
    bool hasPointers() /*const*/;

    void accept(Visitor *v) { v->visit(this); }
};

class TypeAArray : public TypeArray
{
public:
    Type *index;                // key type
    Loc loc;
    Scope *sc;

    TypeAArray(Type *t, Type *index);
    static TypeAArray *create(Type *t, Type *index);
    const char *kind();
    Type *syntaxCopy();
    d_uns64 size(Loc loc);
    Type *semantic(Loc loc, Scope *sc);
    void resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid = false);
    Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    Expression *defaultInit(Loc loc);
    bool isZeroInit(Loc loc) /*const*/;
    bool isBoolean() /*const*/;
    bool hasPointers() /*const*/;
    MATCH implicitConvTo(Type *to);
    MATCH constConv(Type *to);

    void accept(Visitor *v) { v->visit(this); }
};

class TypePointer : public TypeNext
{
public:
    TypePointer(Type *t);
    static TypePointer *create(Type *t);
    const char *kind();
    Type *syntaxCopy();
    Type *semantic(Loc loc, Scope *sc);
    d_uns64 size(Loc loc) /*const*/;
    MATCH implicitConvTo(Type *to);
    MATCH constConv(Type *to);
    bool isscalar() /*const*/;
    Expression *defaultInit(Loc loc);
    bool isZeroInit(Loc loc) /*const*/;
    bool hasPointers() /*const*/;

    void accept(Visitor *v) { v->visit(this); }
};

class TypeReference : public TypeNext
{
public:
    TypeReference(Type *t);
    const char *kind();
    Type *syntaxCopy();
    Type *semantic(Loc loc, Scope *sc);
    d_uns64 size(Loc loc) /*const*/;
    Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    Expression *defaultInit(Loc loc);
    bool isZeroInit(Loc loc) /*const*/;
    void accept(Visitor *v) { v->visit(this); }
};

enum RET
{
    RETregs     = 1,    // returned in registers
    RETstack    = 2     // returned on stack
};

enum TRUST
{
    TRUSTdefault = 0,
    TRUSTsystem = 1,    // @system (same as TRUSTdefault)
    TRUSTtrusted = 2,   // @trusted
    TRUSTsafe = 3       // @safe
};

// in hdrgen.c
void trustToBuffer(OutBuffer *buf, TRUST trust);
const char *trustToChars(TRUST trust);

enum TRUSTformat
{
    TRUSTformatDefault,  // do not emit @system when trust == TRUSTdefault
    TRUSTformatSystem    // emit @system when trust == TRUSTdefault
};

enum PURE
{
    PUREimpure = 0,     // not pure at all
    PUREfwdref = 1,     // it's pure, but not known which level yet
    PUREweak = 2,       // no mutable globals are read or written
    PUREconst = 3,      // parameters are values or const
    PUREstrong = 4      // parameters are values or immutable
};

class Parameter : public ASTNode
{
public:
    StorageClass storageClass;
    Type *type;
    Identifier *ident;
    Expression *defaultArg;

    Parameter(StorageClass storageClass, Type *type, Identifier *ident, Expression *defaultArg);
    static Parameter *create(StorageClass storageClass, Type *type, Identifier *ident, Expression *defaultArg);
    Parameter *syntaxCopy();
    Type *isLazyArray();
    // kludge for template.isType()
    int dyncast() const { return DYNCAST_PARAMETER; }
    void accept(Visitor *v) { v->visit(this); }

    static Parameters *arraySyntaxCopy(Parameters *parameters);
    static size_t dim(Parameters *parameters);
    static Parameter *getNth(Parameters *parameters, d_size_t nth, d_size_t *pn = NULL);
    const char *toChars();
    bool isCovariant(bool returnByRef, const Parameter *p) const;
    static bool isCovariantScope(bool returnByRef, StorageClass from, StorageClass to);
};

struct ParameterList
{
    Parameters *parameters;
    VarArg varargs;

    ParameterList(Parameters *parameters = NULL, VarArg varargs = VARARGnone);

    size_t length();
    Parameter *operator[](size_t i) { return Parameter::getNth(parameters, i); }
};

class TypeFunction : public TypeNext
{
public:
    // .next is the return type

    ParameterList parameterList;     // function parameters

    bool isnothrow;     // true: nothrow
    bool isnogc;        // true: is @nogc
    bool isproperty;    // can be called without parentheses
    bool isref;         // true: returns a reference
    bool isreturn;      // true: 'this' is returned by ref
    bool isscope;       // true: 'this' is scope
    bool isscopeinferred; // true: 'this' is scope from inference
    LINK linkage;  // calling convention
    TRUST trust;   // level of trust
    PURE purity;   // PURExxxx
    unsigned char iswild;   // bit0: inout on params, bit1: inout on qualifier
    Expressions *fargs; // function arguments

    int inuse;

    TypeFunction(const ParameterList &pl, Type *treturn, LINK linkage, StorageClass stc = 0);
    static TypeFunction *create(Parameters *parameters, Type *treturn, VarArg varargs, LINK linkage, StorageClass stc = 0);
    const char *kind();
    Type *syntaxCopy();
    Type *semantic(Loc loc, Scope *sc);
    void purityLevel();
    bool hasLazyParameters();
    bool isDstyleVariadic() const;
    bool parameterEscapes(Parameter *p);
    StorageClass parameterStorageClass(Parameter *p);
    Type *addStorageClass(StorageClass stc);

    /** For each active attribute (ref/const/nogc/etc) call fp with a void* for the
    work param and a string representation of the attribute. */
    int attributesApply(void *param, int (*fp)(void *, const char *), TRUSTformat trustFormat = TRUSTformatDefault);

    Type *substWildTo(unsigned mod);
    MATCH callMatch(Type *tthis, Expressions *toargs, int flag = 0);
    bool checkRetType(Loc loc);

    Expression *defaultInit(Loc loc) /*const*/;
    void accept(Visitor *v) { v->visit(this); }
};

class TypeDelegate : public TypeNext
{
public:
    // .next is a TypeFunction

    TypeDelegate(Type *t);
    static TypeDelegate *create(Type *t);
    const char *kind();
    Type *syntaxCopy();
    Type *semantic(Loc loc, Scope *sc);
    Type *addStorageClass(StorageClass stc);
    d_uns64 size(Loc loc) /*const*/;
    unsigned alignsize() /*const*/;
    MATCH implicitConvTo(Type *to);
    Expression *defaultInit(Loc loc);
    bool isZeroInit(Loc loc) /*const*/;
    bool isBoolean() /*const*/;
    Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    bool hasPointers() /*const*/;

    void accept(Visitor *v) { v->visit(this); }
};

class TypeTraits : public Type
{
public:
    Loc loc;
    /// The expression to resolve as type or symbol.
    TraitsExp *exp;
    /// The symbol when exp doesn't represent a type.
    Dsymbol *sym;

    TypeTraits(const Loc &loc, TraitsExp *exp);
    Type *syntaxCopy();
    Type *semantic(Loc loc, Scope *sc);
    void resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid = false);
    d_uns64 size(Loc loc);
    void accept(Visitor *v) { v->visit(this); }
};

class TypeQualified : public Type
{
public:
    Loc loc;
    // array of Identifier and TypeInstance,
    // representing ident.ident!tiargs.ident. ... etc.
    Objects idents;

    TypeQualified(TY ty, Loc loc);
    void syntaxCopyHelper(TypeQualified *t);
    void addIdent(Identifier *ident);
    void addInst(TemplateInstance *inst);
    void addIndex(RootObject *expr);
    d_uns64 size(Loc loc);

    void resolveTupleIndex(Loc loc, Scope *sc, Dsymbol *s,
        Expression **pe, Type **pt, Dsymbol **ps, RootObject *oindex);
    void resolveHelper(Loc loc, Scope *sc, Dsymbol *s, Dsymbol *scopesym,
        Expression **pe, Type **pt, Dsymbol **ps, bool intypeid = false);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeIdentifier : public TypeQualified
{
public:
    Identifier *ident;
    Dsymbol *originalSymbol; // The symbol representing this identifier, before alias resolution

    TypeIdentifier(Loc loc, Identifier *ident);
    const char *kind();
    Type *syntaxCopy();
    void resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid = false);
    Dsymbol *toDsymbol(Scope *sc);
    Type *semantic(Loc loc, Scope *sc);
    void accept(Visitor *v) { v->visit(this); }
};

/* Similar to TypeIdentifier, but with a TemplateInstance as the root
 */
class TypeInstance : public TypeQualified
{
public:
    TemplateInstance *tempinst;

    TypeInstance(Loc loc, TemplateInstance *tempinst);
    const char *kind();
    Type *syntaxCopy();
    void resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid = false);
    Type *semantic(Loc loc, Scope *sc);
    Dsymbol *toDsymbol(Scope *sc);
    void accept(Visitor *v) { v->visit(this); }
};

class TypeTypeof : public TypeQualified
{
public:
    Expression *exp;
    int inuse;

    TypeTypeof(Loc loc, Expression *exp);
    const char *kind();
    Type *syntaxCopy();
    Dsymbol *toDsymbol(Scope *sc);
    void resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid = false);
    Type *semantic(Loc loc, Scope *sc);
    d_uns64 size(Loc loc);
    void accept(Visitor *v) { v->visit(this); }
};

class TypeReturn : public TypeQualified
{
public:
    TypeReturn(Loc loc);
    const char *kind();
    Type *syntaxCopy();
    Dsymbol *toDsymbol(Scope *sc);
    void resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid = false);
    Type *semantic(Loc loc, Scope *sc);
    void accept(Visitor *v) { v->visit(this); }
};

// Whether alias this dependency is recursive or not.
enum AliasThisRec
{
    RECno = 0,      // no alias this recursion
    RECyes = 1,     // alias this has recursive dependency
    RECfwdref = 2,  // not yet known
    RECtypeMask = 3,// mask to read no/yes/fwdref

    RECtracing = 0x4, // mark in progress of implicitConvTo/deduceWild
    RECtracingDT = 0x8  // mark in progress of deduceType
};

class TypeStruct : public Type
{
public:
    StructDeclaration *sym;
    AliasThisRec att;
    CPPMANGLE cppmangle;

    TypeStruct(StructDeclaration *sym);
    static TypeStruct *create(StructDeclaration *sym);
    const char *kind();
    d_uns64 size(Loc loc);
    unsigned alignsize();
    Type *syntaxCopy();
    Type *semantic(Loc loc, Scope *sc);
    Dsymbol *toDsymbol(Scope *sc);
    Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    structalign_t alignment();
    Expression *defaultInit(Loc loc);
    Expression *defaultInitLiteral(Loc loc);
    bool isZeroInit(Loc loc) /*const*/;
    bool isAssignable();
    bool isBoolean() /*const*/;
    bool needsDestruction() /*const*/;
    bool needsNested();
    bool hasPointers();
    bool hasVoidInitPointers();
    MATCH implicitConvTo(Type *to);
    MATCH constConv(Type *to);
    unsigned char deduceWild(Type *t, bool isRef);
    Type *toHeadMutable();

    void accept(Visitor *v) { v->visit(this); }
};

class TypeEnum : public Type
{
public:
    EnumDeclaration *sym;

    TypeEnum(EnumDeclaration *sym);
    const char *kind();
    Type *syntaxCopy();
    d_uns64 size(Loc loc);
    unsigned alignsize();
    Type *semantic(Loc loc, Scope *sc);
    Dsymbol *toDsymbol(Scope *sc);
    Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    Expression *getProperty(Loc loc, Identifier *ident, int flag);
    bool isintegral();
    bool isfloating();
    bool isreal();
    bool isimaginary();
    bool iscomplex();
    bool isscalar();
    bool isunsigned();
    bool isBoolean();
    bool isString();
    bool isAssignable();
    bool needsDestruction();
    bool needsNested();
    MATCH implicitConvTo(Type *to);
    MATCH constConv(Type *to);
    Type *toBasetype();
    Expression *defaultInit(Loc loc);
    bool isZeroInit(Loc loc);
    bool hasPointers();
    bool hasVoidInitPointers();
    Type *nextOf();

    void accept(Visitor *v) { v->visit(this); }
};

class TypeClass : public Type
{
public:
    ClassDeclaration *sym;
    AliasThisRec att;
    CPPMANGLE cppmangle;

    TypeClass(ClassDeclaration *sym);
    const char *kind();
    d_uns64 size(Loc loc) /*const*/;
    Type *syntaxCopy();
    Type *semantic(Loc loc, Scope *sc);
    Dsymbol *toDsymbol(Scope *sc);
    Expression *dotExp(Scope *sc, Expression *e, Identifier *ident, int flag);
    ClassDeclaration *isClassHandle();
    bool isBaseOf(Type *t, int *poffset);
    MATCH implicitConvTo(Type *to);
    MATCH constConv(Type *to);
    unsigned char deduceWild(Type *t, bool isRef);
    Type *toHeadMutable();
    Expression *defaultInit(Loc loc);
    bool isZeroInit(Loc loc) /*const*/;
    bool isscope() /*const*/;
    bool isBoolean() /*const*/;
    bool hasPointers() /*const*/;

    void accept(Visitor *v) { v->visit(this); }
};

class TypeTuple : public Type
{
public:
    Parameters *arguments;      // types making up the tuple

    TypeTuple(Parameters *arguments);
    TypeTuple(Expressions *exps);
    static TypeTuple *create(Parameters *arguments);
    TypeTuple();
    TypeTuple(Type *t1);
    TypeTuple(Type *t1, Type *t2);
    const char *kind();
    Type *syntaxCopy();
    Type *semantic(Loc loc, Scope *sc);
    bool equals(RootObject *o);
    Expression *getProperty(Loc loc, Identifier *ident, int flag);
    Expression *defaultInit(Loc loc);
    void accept(Visitor *v) { v->visit(this); }
};

class TypeSlice : public TypeNext
{
public:
    Expression *lwr;
    Expression *upr;

    TypeSlice(Type *next, Expression *lwr, Expression *upr);
    const char *kind();
    Type *syntaxCopy();
    Type *semantic(Loc loc, Scope *sc);
    void resolve(Loc loc, Scope *sc, Expression **pe, Type **pt, Dsymbol **ps, bool intypeid = false);
    void accept(Visitor *v) { v->visit(this); }
};

class TypeNull : public Type
{
public:
    TypeNull();
    const char *kind();

    Type *syntaxCopy();
    MATCH implicitConvTo(Type *to);
    bool isBoolean() /*const*/;

    d_uns64 size(Loc loc) /*const*/;
    Expression *defaultInit(Loc loc) /*const*/;
    void accept(Visitor *v) { v->visit(this); }
};

/**************************************************************/

bool arrayTypeCompatible(Loc loc, Type *t1, Type *t2);
bool arrayTypeCompatibleWithoutCasting(Type *t1, Type *t2);
