
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2022 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/mtype.h
 */

#pragma once

#include "root/dcompat.h" // for d_size_t

#include "arraytypes.h"
#include "ast_node.h"
#include "globals.h"
#include "visitor.h"

struct Scope;
class AggregateDeclaration;
class Identifier;
class Expression;
class StructDeclaration;
class ClassDeclaration;
class EnumDeclaration;
class TypeInfoDeclaration;
class Dsymbol;
class TemplateInstance;
class TemplateDeclaration;

class TypeBasic;
class Parameter;

// Back end
#ifdef IN_GCC
typedef union tree_node type;
#else
typedef struct TYPE type;
#endif

void semanticTypeInfo(Scope *sc, Type *t);

Type *typeSemantic(Type *t, const Loc &loc, Scope *sc);
Type *merge(Type *type);

enum class TY : uint8_t
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
    Tmixin,
    Tnoreturn,
    TMAX
};

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

enum class Covariant
{
    distinct = 0,
    yes = 1,
    no = 2,
    fwdref = 3,
};

enum VarArgValues
{
    VARARGnone     = 0,  /// fixed number of arguments
    VARARGvariadic = 1,  /// T t, ...)  can be C-style (core.stdc.stdarg) or D-style (core.vararg)
    VARARGtypesafe = 2   /// T t ...) typesafe https://dlang.org/spec/function.html#typesafe_variadic_functions
                         ///   or https://dlang.org/spec/function.html#typesafe_variadic_functions
};
typedef unsigned char VarArg;

class Type : public ASTNode
{
public:
    TY ty;
    MOD mod;  // modifiers MODxxxx
    char *deco;

private:
    void* mcache;

public:
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
    static Type *tnoreturn;             // for bottom type typeof(*null)

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

    static Type *basic[(int)TY::TMAX];

    virtual const char *kind();
    Type *copy() const;
    virtual Type *syntaxCopy();
    bool equals(const RootObject *o) const;
    bool equivalent(Type *t);
    // kludge for template.isType()
    DYNCAST dyncast() const { return DYNCAST_TYPE; }
    size_t getUniqueID() const;
    Covariant covariant(Type *t, StorageClass *pstc = NULL);
    const char *toChars() const;
    char *toPrettyChars(bool QualifyTypes = false);
    static void _init();

    d_uns64 size();
    virtual d_uns64 size(const Loc &loc);
    virtual unsigned alignsize();
    Type *trySemantic(const Loc &loc, Scope *sc);
    Type *merge2();
    void modToBuffer(OutBuffer *buf) const;
    char *modToChars() const;

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
    virtual void checkDeprecated(const Loc &loc, Scope *sc);
    bool isConst() const       { return (mod & MODconst) != 0; }
    bool isImmutable() const   { return (mod & MODimmutable) != 0; }
    bool isMutable() const     { return (mod & (MODconst | MODimmutable | MODwild)) == 0; }
    bool isShared() const      { return (mod & MODshared) != 0; }
    bool isSharedConst() const { return (mod & (MODshared | MODconst)) == (MODshared | MODconst); }
    bool isWild() const        { return (mod & MODwild) != 0; }
    bool isWildConst() const   { return (mod & MODwildconst) == MODwildconst; }
    bool isSharedWild() const  { return (mod & (MODshared | MODwild)) == (MODshared | MODwild); }
    bool isNaked() const       { return mod == 0; }
    Type *nullAttributes() const;
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
    bool hasDeprecatedAliasThis();
    Type *aliasthisOf();
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
    Type *toBasetype();
    virtual bool isBaseOf(Type *t, int *poffset);
    virtual MATCH implicitConvTo(Type *to);
    virtual MATCH constConv(Type *to);
    virtual unsigned char deduceWild(Type *t, bool isRef);
    virtual Type *substWildTo(unsigned mod);

    Type *unqualify(unsigned m);

    virtual Type *toHeadMutable();
    virtual ClassDeclaration *isClassHandle();
    virtual structalign_t alignment();
    virtual Expression *defaultInitLiteral(const Loc &loc);
    virtual bool isZeroInit(const Loc &loc = Loc());                // if initializer is 0
    Identifier *getTypeInfoIdent();
    virtual int hasWild() const;
    virtual bool hasPointers();
    virtual bool hasVoidInitPointers();
    virtual bool hasInvariant();
    virtual Type *nextOf();
    Type *baseElemOf();
    uinteger_t sizemask();
    virtual bool needsDestruction();
    virtual bool needsCopyOrPostblit();
    virtual bool needsNested();

    TypeFunction *toTypeFunction();

    // For eliminating dynamic_cast
    virtual TypeBasic *isTypeBasic();
    TypeFunction *isPtrToFunction();
    TypeFunction *isFunction_Delegate_PtrToFunction();
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
    TypeMixin *isTypeMixin();
    TypeTraits *isTypeTraits();
    TypeNoreturn *isTypeNoreturn();
    TypeTag *isTypeTag();

    void accept(Visitor *v) { v->visit(this); }
};

class TypeError : public Type
{
public:
    const char *kind();
    TypeError *syntaxCopy();

    d_uns64 size(const Loc &loc);
    Expression *defaultInitLiteral(const Loc &loc);
    void accept(Visitor *v) { v->visit(this); }
};

class TypeNext : public Type
{
public:
    Type *next;

    void checkDeprecated(const Loc &loc, Scope *sc);
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

    const char *kind();
    TypeBasic *syntaxCopy();
    d_uns64 size(const Loc &loc) /*const*/;
    unsigned alignsize();
    bool isintegral();
    bool isfloating() /*const*/;
    bool isreal() /*const*/;
    bool isimaginary() /*const*/;
    bool iscomplex() /*const*/;
    bool isscalar() /*const*/;
    bool isunsigned() /*const*/;
    MATCH implicitConvTo(Type *to);
    bool isZeroInit(const Loc &loc) /*const*/;

    // For eliminating dynamic_cast
    TypeBasic *isTypeBasic();
    void accept(Visitor *v) { v->visit(this); }
};

class TypeVector : public Type
{
public:
    Type *basetype;

    static TypeVector *create(Type *basetype);
    const char *kind();
    TypeVector *syntaxCopy();
    d_uns64 size(const Loc &loc);
    unsigned alignsize();
    bool isintegral();
    bool isfloating();
    bool isscalar();
    bool isunsigned();
    bool isBoolean() /*const*/;
    MATCH implicitConvTo(Type *to);
    Expression *defaultInitLiteral(const Loc &loc);
    TypeBasic *elementType();
    bool isZeroInit(const Loc &loc);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeArray : public TypeNext
{
public:
    void accept(Visitor *v) { v->visit(this); }
};

// Static array, one with a fixed dimension
class TypeSArray : public TypeArray
{
public:
    Expression *dim;

    const char *kind();
    TypeSArray *syntaxCopy();
    bool isIncomplete();
    d_uns64 size(const Loc &loc);
    unsigned alignsize();
    bool isString();
    bool isZeroInit(const Loc &loc);
    structalign_t alignment();
    MATCH constConv(Type *to);
    MATCH implicitConvTo(Type *to);
    Expression *defaultInitLiteral(const Loc &loc);
    bool hasPointers();
    bool hasInvariant();
    bool needsDestruction();
    bool needsCopyOrPostblit();
    bool needsNested();

    void accept(Visitor *v) { v->visit(this); }
};

// Dynamic array, no dimension
class TypeDArray : public TypeArray
{
public:
    const char *kind();
    TypeDArray *syntaxCopy();
    d_uns64 size(const Loc &loc) /*const*/;
    unsigned alignsize() /*const*/;
    bool isString();
    bool isZeroInit(const Loc &loc) /*const*/;
    bool isBoolean() /*const*/;
    MATCH implicitConvTo(Type *to);
    bool hasPointers() /*const*/;

    void accept(Visitor *v) { v->visit(this); }
};

class TypeAArray : public TypeArray
{
public:
    Type *index;                // key type
    Loc loc;

    static TypeAArray *create(Type *t, Type *index);
    const char *kind();
    TypeAArray *syntaxCopy();
    d_uns64 size(const Loc &loc);
    bool isZeroInit(const Loc &loc) /*const*/;
    bool isBoolean() /*const*/;
    bool hasPointers() /*const*/;
    MATCH implicitConvTo(Type *to);
    MATCH constConv(Type *to);

    void accept(Visitor *v) { v->visit(this); }
};

class TypePointer : public TypeNext
{
public:
    static TypePointer *create(Type *t);
    const char *kind();
    TypePointer *syntaxCopy();
    d_uns64 size(const Loc &loc) /*const*/;
    MATCH implicitConvTo(Type *to);
    MATCH constConv(Type *to);
    bool isscalar() /*const*/;
    bool isZeroInit(const Loc &loc) /*const*/;
    bool hasPointers() /*const*/;

    void accept(Visitor *v) { v->visit(this); }
};

class TypeReference : public TypeNext
{
public:
    const char *kind();
    TypeReference *syntaxCopy();
    d_uns64 size(const Loc &loc) /*const*/;
    bool isZeroInit(const Loc &loc) /*const*/;
    void accept(Visitor *v) { v->visit(this); }
};

enum RET
{
    RETregs     = 1,    // returned in registers
    RETstack    = 2     // returned on stack
};

enum class TRUST : unsigned char
{
    default_ = 0,
    system = 1,    // @system (same as TRUSTdefault)
    trusted = 2,   // @trusted
    safe = 3       // @safe
};

enum TRUSTformat
{
    TRUSTformatDefault,  // do not emit @system when trust == TRUSTdefault
    TRUSTformatSystem    // emit @system when trust == TRUSTdefault
};

enum class PURE : unsigned char
{
    impure = 0,     // not pure at all
    fwdref = 1,     // it's pure, but not known which level yet
    weak = 2,       // no mutable globals are read or written
    const_ = 3,     // parameters are values or const
    strong = 4      // parameters are values or immutable
};

class Parameter : public ASTNode
{
public:
    StorageClass storageClass;
    Type *type;
    Identifier *ident;
    Expression *defaultArg;
    UserAttributeDeclaration *userAttribDecl;   // user defined attributes

    static Parameter *create(StorageClass storageClass, Type *type, Identifier *ident,
                             Expression *defaultArg, UserAttributeDeclaration *userAttribDecl);
    Parameter *syntaxCopy();
    Type *isLazyArray();
    // kludge for template.isType()
    DYNCAST dyncast() const { return DYNCAST_PARAMETER; }
    void accept(Visitor *v) { v->visit(this); }

    static size_t dim(Parameters *parameters);
    static Parameter *getNth(Parameters *parameters, d_size_t nth);
    const char *toChars() const;
    bool isCovariant(bool returnByRef, const Parameter *p, bool previewIn) const;
};

struct ParameterList
{
    Parameters* parameters;
    StorageClass stc;
    VarArg varargs;
    bool hasIdentifierList; // true if C identifier-list style

    size_t length();
    Parameter *operator[](size_t i) { return Parameter::getNth(parameters, i); }
};

class TypeFunction : public TypeNext
{
public:
    // .next is the return type

    ParameterList parameterList; // function parameters
    LINK linkage;                // calling convention
    unsigned funcFlags;
    TRUST trust;                 // level of trust
    PURE purity;                 // PURExxxx
    char inuse;
    Expressions *fargs;          // function arguments

    static TypeFunction *create(Parameters *parameters, Type *treturn, VarArg varargs, LINK linkage, StorageClass stc = 0);
    const char *kind();
    TypeFunction *syntaxCopy();
    void purityLevel();
    bool hasLazyParameters();
    bool isDstyleVariadic() const;
    bool parameterEscapes(Parameter *p);
    StorageClass parameterStorageClass(Parameter *p);
    Type *addStorageClass(StorageClass stc);

    Type *substWildTo(unsigned mod);
    MATCH constConv(Type *to);

    bool isnothrow() const;
    void isnothrow(bool v);
    bool isnogc() const;
    void isnogc(bool v);
    bool isproperty() const;
    void isproperty(bool v);
    bool isref() const;
    void isref(bool v);
    bool isreturn() const;
    void isreturn(bool v);
    bool isScopeQual() const;
    void isScopeQual(bool v);
    bool isreturninferred() const;
    void isreturninferred(bool v);
    bool isscopeinferred() const;
    void isscopeinferred(bool v);
    bool islive() const;
    void islive(bool v);
    bool incomplete() const;
    void incomplete(bool v);
    bool isInOutParam() const;
    void isInOutParam(bool v);
    bool isInOutQual() const;
    void isInOutQual(bool v);
    bool iswild() const;

    void accept(Visitor *v) { v->visit(this); }
};

class TypeDelegate : public TypeNext
{
public:
    // .next is a TypeFunction

    static TypeDelegate *create(TypeFunction *t);
    const char *kind();
    TypeDelegate *syntaxCopy();
    Type *addStorageClass(StorageClass stc);
    d_uns64 size(const Loc &loc) /*const*/;
    unsigned alignsize() /*const*/;
    MATCH implicitConvTo(Type *to);
    bool isZeroInit(const Loc &loc) /*const*/;
    bool isBoolean() /*const*/;
    bool hasPointers() /*const*/;

    void accept(Visitor *v) { v->visit(this); }
};

class TypeTraits : public Type
{
    Loc loc;
    /// The expression to resolve as type or symbol.
    TraitsExp *exp;
    /// The symbol when exp doesn't represent a type.
    Dsymbol *sym;

    const char *kind();
    TypeTraits *syntaxCopy();
    d_uns64 size(const Loc &loc);
    Dsymbol *toDsymbol(Scope *sc);
    void accept(Visitor *v) { v->visit(this); }
};

class TypeMixin : public Type
{
    Loc loc;
    Expressions *exps;
    RootObject *obj;

    const char *kind();
    TypeMixin *syntaxCopy();
    Dsymbol *toDsymbol(Scope *sc);
    void accept(Visitor *v) { v->visit(this); }
};

class TypeQualified : public Type
{
public:
    Loc loc;
    // array of Identifier and TypeInstance,
    // representing ident.ident!tiargs.ident. ... etc.
    Objects idents;

    void syntaxCopyHelper(TypeQualified *t);
    void addIdent(Identifier *ident);
    void addInst(TemplateInstance *inst);
    void addIndex(RootObject *expr);
    d_uns64 size(const Loc &loc);

    void accept(Visitor *v) { v->visit(this); }
};

class TypeIdentifier : public TypeQualified
{
public:
    Identifier *ident;
    Dsymbol *originalSymbol; // The symbol representing this identifier, before alias resolution

    static TypeIdentifier *create(const Loc &loc, Identifier *ident);
    const char *kind();
    TypeIdentifier *syntaxCopy();
    Dsymbol *toDsymbol(Scope *sc);
    void accept(Visitor *v) { v->visit(this); }
};

/* Similar to TypeIdentifier, but with a TemplateInstance as the root
 */
class TypeInstance : public TypeQualified
{
public:
    TemplateInstance *tempinst;

    const char *kind();
    TypeInstance *syntaxCopy();
    Dsymbol *toDsymbol(Scope *sc);
    void accept(Visitor *v) { v->visit(this); }
};

class TypeTypeof : public TypeQualified
{
public:
    Expression *exp;
    int inuse;

    const char *kind();
    TypeTypeof *syntaxCopy();
    Dsymbol *toDsymbol(Scope *sc);
    d_uns64 size(const Loc &loc);
    void accept(Visitor *v) { v->visit(this); }
};

class TypeReturn : public TypeQualified
{
public:
    const char *kind();
    TypeReturn *syntaxCopy();
    Dsymbol *toDsymbol(Scope *sc);
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
    bool inuse;

    static TypeStruct *create(StructDeclaration *sym);
    const char *kind();
    d_uns64 size(const Loc &loc);
    unsigned alignsize();
    TypeStruct *syntaxCopy();
    Dsymbol *toDsymbol(Scope *sc);
    structalign_t alignment();
    Expression *defaultInitLiteral(const Loc &loc);
    bool isZeroInit(const Loc &loc);
    bool isAssignable();
    bool isBoolean() /*const*/;
    bool needsDestruction() /*const*/;
    bool needsCopyOrPostblit();
    bool needsNested();
    bool hasPointers();
    bool hasVoidInitPointers();
    bool hasInvariant();
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

    const char *kind();
    TypeEnum *syntaxCopy();
    d_uns64 size(const Loc &loc);
    unsigned alignsize();
    Type *memType(const Loc &loc = Loc());
    Dsymbol *toDsymbol(Scope *sc);
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
    bool needsCopyOrPostblit();
    bool needsNested();
    MATCH implicitConvTo(Type *to);
    MATCH constConv(Type *to);
    bool isZeroInit(const Loc &loc);
    bool hasPointers();
    bool hasVoidInitPointers();
    bool hasInvariant();
    Type *nextOf();

    void accept(Visitor *v) { v->visit(this); }
};

class TypeClass : public Type
{
public:
    ClassDeclaration *sym;
    AliasThisRec att;
    CPPMANGLE cppmangle;

    const char *kind();
    d_uns64 size(const Loc &loc) /*const*/;
    TypeClass *syntaxCopy();
    Dsymbol *toDsymbol(Scope *sc);
    ClassDeclaration *isClassHandle();
    bool isBaseOf(Type *t, int *poffset);
    MATCH implicitConvTo(Type *to);
    MATCH constConv(Type *to);
    unsigned char deduceWild(Type *t, bool isRef);
    Type *toHeadMutable();
    bool isZeroInit(const Loc &loc) /*const*/;
    bool isscope() /*const*/;
    bool isBoolean() /*const*/;
    bool hasPointers() /*const*/;

    void accept(Visitor *v) { v->visit(this); }
};

class TypeTuple : public Type
{
public:
    // 'logically immutable' cached global - don't modify (neither pointer nor pointee)!
    static TypeTuple *empty;

    Parameters *arguments;      // types making up the tuple

    static TypeTuple *create(Parameters *arguments);
    static TypeTuple *create();
    static TypeTuple *create(Type *t1);
    static TypeTuple *create(Type *t1, Type *t2);
    const char *kind();
    TypeTuple *syntaxCopy();
    bool equals(const RootObject *o) const;
    void accept(Visitor *v) { v->visit(this); }
};

class TypeSlice : public TypeNext
{
public:
    Expression *lwr;
    Expression *upr;

    const char *kind();
    TypeSlice *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};

class TypeNull : public Type
{
public:
    const char *kind();

    TypeNull *syntaxCopy();
    MATCH implicitConvTo(Type *to);
    bool isBoolean() /*const*/;

    d_uns64 size(const Loc &loc) /*const*/;
    void accept(Visitor *v) { v->visit(this); }
};

class TypeNoreturn final : public Type
{
public:
    const char *kind();
    TypeNoreturn *syntaxCopy();
    MATCH implicitConvTo(Type* to);
    MATCH constConv(Type* to);
    bool isBoolean() /* const */;
    d_uns64 size(const Loc& loc) /* const */;
    unsigned alignsize();

    void accept(Visitor *v) { v->visit(this); }
};

class TypeTag final : public Type
{
public:
    TypeTag *syntaxCopy();

    void accept(Visitor *v) { v->visit(this); }
};

/**************************************************************/

bool arrayTypeCompatibleWithoutCasting(Type *t1, Type *t2);

// If the type is a class or struct, returns the symbol for it, else null.
AggregateDeclaration *isAggregate(Type *t);
