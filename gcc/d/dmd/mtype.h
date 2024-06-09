
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
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
#include "expression.h"
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

namespace dmd
{
    Type *typeSemantic(Type *t, const Loc &loc, Scope *sc);
    Type *merge(Type *type);
}

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

#define SIZE_INVALID (~(uinteger_t)0)   // error return from size() functions


/**
 * type modifiers
 * pick this order of numbers so switch statements work better
 */
enum MODFlags
{
    MODnone      = 0, // default (mutable)
    MODconst     = 1, // type is const
    MODimmutable = 4, // type is immutable
    MODshared    = 2, // type is shared
    MODwild      = 8, // type is wild
    MODwildconst = (MODwild | MODconst), // type is wild const
    MODmutable   = 0x10       // type is mutable (only used in wildcard matching)
};
typedef unsigned char MOD;

enum VarArgValues
{
    VARARGnone     = 0,  /// fixed number of arguments
    VARARGvariadic = 1,  /// T t, ...)  can be C-style (core.stdc.stdarg) or D-style (core.vararg)
    VARARGtypesafe = 2,  /// T t ...) typesafe https://dlang.org/spec/function.html#typesafe_variadic_functions
                         ///   or https://dlang.org/spec/function.html#typesafe_variadic_functions
    VARARGKRvariadic = 3 /// K+R C style variadics (no function prototype)
};
typedef unsigned char VarArg;

enum class Covariant
{
    distinct = 0, /// types are distinct
    yes = 1,      /// types are covariant
    no = 2,       /// arguments match as far as overloading goes, but types are not covariant
    fwdref = 3,   /// cannot determine covariance because of forward references
};

class Type : public ASTNode
{
public:
    TY ty;
    MOD mod;  // modifiers MODxxxx
    char *deco;
    void* mcache;
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
    bool equals(const RootObject * const o) const override;
    // kludge for template.isType()
    DYNCAST dyncast() const override final { return DYNCAST_TYPE; }
    size_t getUniqueID() const;
    const char *toChars() const override;
    char *toPrettyChars(bool QualifyTypes = false);
    static void _init();

    uinteger_t size();
    virtual uinteger_t size(const Loc &loc);
    virtual unsigned alignsize();
    void modToBuffer(OutBuffer& buf) const;
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
    bool hasDeprecatedAliasThis();
    virtual Type *makeConst();
    virtual Type *makeImmutable();
    virtual Type *makeShared();
    virtual Type *makeSharedConst();
    virtual Type *makeWild();
    virtual Type *makeWildConst();
    virtual Type *makeSharedWild();
    virtual Type *makeSharedWildConst();
    virtual Type *makeMutable();
    Type *toBasetype();
    virtual MATCH implicitConvTo(Type *to);
    virtual MATCH constConv(Type *to);
    virtual unsigned char deduceWild(Type *t, bool isRef);

    virtual ClassDeclaration *isClassHandle();
    virtual structalign_t alignment();
    virtual Expression *defaultInitLiteral(const Loc &loc);
    virtual bool isZeroInit(const Loc &loc = Loc()); // if initializer is 0
    virtual int hasWild() const;
    virtual bool hasVoidInitPointers();
    virtual bool hasSystemFields();
    virtual bool hasInvariant();
    virtual Type *nextOf();
    Type *baseElemOf();
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

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeError final : public Type
{
public:
    const char *kind() override;
    TypeError *syntaxCopy() override;

    uinteger_t size(const Loc &loc) override;
    Expression *defaultInitLiteral(const Loc &loc) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class TypeNext : public Type
{
public:
    Type *next;

    int hasWild() const override final;
    Type *nextOf() override final;
    Type *makeConst() override final;
    Type *makeImmutable() override final;
    Type *makeShared() override final;
    Type *makeSharedConst() override final;
    Type *makeWild() override final;
    Type *makeWildConst() override final;
    Type *makeSharedWild() override final;
    Type *makeSharedWildConst() override final;
    Type *makeMutable() override final;
    MATCH constConv(Type *to) override;
    unsigned char deduceWild(Type *t, bool isRef) override final;
    void transitive();
    void accept(Visitor *v) override { v->visit(this); }
};

class TypeBasic final : public Type
{
public:
    const char *dstring;
    unsigned flags;

    const char *kind() override;
    TypeBasic *syntaxCopy() override;
    uinteger_t size(const Loc &loc) override;
    unsigned alignsize() override;
    bool isintegral() override;
    bool isfloating() override;
    bool isreal() override;
    bool isimaginary() override;
    bool iscomplex() override;
    bool isscalar() override;
    bool isunsigned() override;
    MATCH implicitConvTo(Type *to) override;
    bool isZeroInit(const Loc &loc) override;

    // For eliminating dynamic_cast
    TypeBasic *isTypeBasic() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class TypeVector final : public Type
{
public:
    Type *basetype;

    static TypeVector *create(Type *basetype);
    const char *kind() override;
    TypeVector *syntaxCopy() override;
    uinteger_t size(const Loc &loc) override;
    unsigned alignsize() override;
    bool isintegral() override;
    bool isfloating() override;
    bool isscalar() override;
    bool isunsigned() override;
    bool isBoolean() override;
    MATCH implicitConvTo(Type *to) override;
    Expression *defaultInitLiteral(const Loc &loc) override;
    TypeBasic *elementType();
    bool isZeroInit(const Loc &loc) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeArray : public TypeNext
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

// Static array, one with a fixed dimension
class TypeSArray final : public TypeArray
{
public:
    Expression *dim;

    const char *kind() override;
    TypeSArray *syntaxCopy() override;
    bool isIncomplete();
    uinteger_t size(const Loc &loc) override;
    unsigned alignsize() override;
    bool isString() override;
    bool isZeroInit(const Loc &loc) override;
    structalign_t alignment() override;
    MATCH constConv(Type *to) override;
    MATCH implicitConvTo(Type *to) override;
    Expression *defaultInitLiteral(const Loc &loc) override;
    bool hasSystemFields() override;
    bool hasVoidInitPointers() override;
    bool hasInvariant() override;
    bool needsDestruction() override;
    bool needsCopyOrPostblit() override;
    bool needsNested() override;

    void accept(Visitor *v) override { v->visit(this); }
};

// Dynamic array, no dimension
class TypeDArray final : public TypeArray
{
public:
    const char *kind() override;
    TypeDArray *syntaxCopy() override;
    uinteger_t size(const Loc &loc) override;
    unsigned alignsize() override;
    bool isString() override;
    bool isZeroInit(const Loc &loc) override;
    bool isBoolean() override;
    MATCH implicitConvTo(Type *to) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeAArray final : public TypeArray
{
public:
    Type *index;                // key type
    Loc loc;

    static TypeAArray *create(Type *t, Type *index);
    const char *kind() override;
    TypeAArray *syntaxCopy() override;
    uinteger_t size(const Loc &loc) override;
    bool isZeroInit(const Loc &loc) override;
    bool isBoolean() override;
    MATCH implicitConvTo(Type *to) override;
    MATCH constConv(Type *to) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class TypePointer final : public TypeNext
{
public:
    static TypePointer *create(Type *t);
    const char *kind() override;
    TypePointer *syntaxCopy() override;
    uinteger_t size(const Loc &loc) override;
    MATCH implicitConvTo(Type *to) override;
    MATCH constConv(Type *to) override;
    bool isscalar() override;
    bool isZeroInit(const Loc &loc) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeReference final : public TypeNext
{
public:
    const char *kind() override;
    TypeReference *syntaxCopy() override;
    uinteger_t size(const Loc &loc) override;
    bool isZeroInit(const Loc &loc) override;
    void accept(Visitor *v) override { v->visit(this); }
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
};

class Parameter final : public ASTNode
{
public:
    Loc loc;
    StorageClass storageClass;
    Type *type;
    Identifier *ident;
    Expression *defaultArg;
    UserAttributeDeclaration *userAttribDecl;   // user defined attributes

    static Parameter *create(const Loc &loc, StorageClass storageClass, Type *type, Identifier *ident,
                             Expression *defaultArg, UserAttributeDeclaration *userAttribDecl);
    Parameter *syntaxCopy();
    Type *isLazyArray();
    bool isLazy() const;
    bool isReference() const;
    // kludge for template.isType()
    DYNCAST dyncast() const override { return DYNCAST_PARAMETER; }
    void accept(Visitor *v) override { v->visit(this); }

    static size_t dim(Parameters *parameters);
    static Parameter *getNth(Parameters *parameters, d_size_t nth);
    const char *toChars() const override;
    bool isCovariant(bool returnByRef, const Parameter *p, bool previewIn) const;
};

struct ParameterList
{
    Parameters* parameters;
    StorageClass stc;
    VarArg varargs;
    d_bool hasIdentifierList; // true if C identifier-list style

    size_t length();
    Parameter *operator[](size_t i) { return Parameter::getNth(parameters, i); }
};

class TypeFunction final : public TypeNext
{
public:
    // .next is the return type

    ParameterList parameterList; // function parameters
    uint16_t bitFields;
    LINK linkage;                // calling convention
    TRUST trust;                 // level of trust
    PURE purity;                 // PURExxxx
    char inuse;
    ArgumentList inferenceArguments; // function arguments

    static TypeFunction *create(Parameters *parameters, Type *treturn, VarArg varargs, LINK linkage, StorageClass stc = 0);
    const char *kind() override;
    TypeFunction *syntaxCopy() override;
    bool hasLazyParameters();
    bool isDstyleVariadic() const;

    MATCH constConv(Type *to) override;

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
    bool isreturnscope() const;
    void isreturnscope(bool v);
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

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeDelegate final : public TypeNext
{
public:
    // .next is a TypeFunction

    static TypeDelegate *create(TypeFunction *t);
    const char *kind() override;
    TypeDelegate *syntaxCopy() override;
    uinteger_t size(const Loc &loc) override;
    unsigned alignsize() override;
    MATCH implicitConvTo(Type *to) override;
    bool isZeroInit(const Loc &loc) override;
    bool isBoolean() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeTraits final : public Type
{
    Loc loc;
    /// The expression to resolve as type or symbol.
    TraitsExp *exp;
    /// Cached type/symbol after semantic analysis.
    RootObject *obj;

    const char *kind() override;
    TypeTraits *syntaxCopy() override;
    uinteger_t size(const Loc &loc) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class TypeMixin final : public Type
{
    Loc loc;
    Expressions *exps;
    RootObject *obj;

    const char *kind() override;
    TypeMixin *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class TypeQualified : public Type
{
public:
    Loc loc;
    // array of Identifier and TypeInstance,
    // representing ident.ident!tiargs.ident. ... etc.
    Objects idents;

    uinteger_t size(const Loc &loc) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeIdentifier final : public TypeQualified
{
public:
    Identifier *ident;
    Dsymbol *originalSymbol; // The symbol representing this identifier, before alias resolution

    static TypeIdentifier *create(const Loc &loc, Identifier *ident);
    const char *kind() override;
    TypeIdentifier *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

/* Similar to TypeIdentifier, but with a TemplateInstance as the root
 */
class TypeInstance final : public TypeQualified
{
public:
    TemplateInstance *tempinst;

    const char *kind() override;
    TypeInstance *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class TypeTypeof final : public TypeQualified
{
public:
    Expression *exp;
    int inuse;

    const char *kind() override;
    TypeTypeof *syntaxCopy() override;
    uinteger_t size(const Loc &loc) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class TypeReturn final : public TypeQualified
{
public:
    const char *kind() override;
    TypeReturn *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
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

class TypeStruct final : public Type
{
public:
    StructDeclaration *sym;
    AliasThisRec att;
    d_bool inuse;

    static TypeStruct *create(StructDeclaration *sym);
    const char *kind() override;
    uinteger_t size(const Loc &loc) override;
    unsigned alignsize() override;
    TypeStruct *syntaxCopy() override;
    structalign_t alignment() override;
    Expression *defaultInitLiteral(const Loc &loc) override;
    bool isZeroInit(const Loc &loc) override;
    bool isAssignable() override;
    bool isBoolean() override;
    bool needsDestruction() override;
    bool needsCopyOrPostblit() override;
    bool needsNested() override;
    bool hasVoidInitPointers() override;
    bool hasSystemFields() override;
    bool hasInvariant() override;
    MATCH implicitConvTo(Type *to) override;
    MATCH constConv(Type *to) override;
    unsigned char deduceWild(Type *t, bool isRef) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeEnum final : public Type
{
public:
    EnumDeclaration *sym;

    const char *kind() override;
    TypeEnum *syntaxCopy() override;
    uinteger_t size(const Loc &loc) override;
    unsigned alignsize() override;
    Type *memType(const Loc &loc);
    bool isintegral() override;
    bool isfloating() override;
    bool isreal() override;
    bool isimaginary() override;
    bool iscomplex() override;
    bool isscalar() override;
    bool isunsigned() override;
    bool isBoolean() override;
    bool isString() override;
    bool isAssignable() override;
    bool needsDestruction() override;
    bool needsCopyOrPostblit() override;
    bool needsNested() override;
    MATCH implicitConvTo(Type *to) override;
    MATCH constConv(Type *to) override;
    bool isZeroInit(const Loc &loc) override;
    bool hasVoidInitPointers() override;
    bool hasSystemFields() override;
    bool hasInvariant() override;
    Type *nextOf() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeClass final : public Type
{
public:
    ClassDeclaration *sym;
    AliasThisRec att;
    CPPMANGLE cppmangle;

    const char *kind() override;
    uinteger_t size(const Loc &loc) override;
    TypeClass *syntaxCopy() override;
    ClassDeclaration *isClassHandle() override;
    MATCH implicitConvTo(Type *to) override;
    MATCH constConv(Type *to) override;
    unsigned char deduceWild(Type *t, bool isRef) override;
    bool isZeroInit(const Loc &loc) override;
    bool isscope() override;
    bool isBoolean() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeTuple final : public Type
{
public:
    // 'logically immutable' cached global - don't modify (neither pointer nor pointee)!
    static TypeTuple *empty;

    Parameters *arguments;      // types making up the tuple

    static TypeTuple *create(Parameters *arguments);
    static TypeTuple *create();
    static TypeTuple *create(Type *t1);
    static TypeTuple *create(Type *t1, Type *t2);
    const char *kind() override;
    TypeTuple *syntaxCopy() override;
    bool equals(const RootObject * const o) const override;
    void accept(Visitor *v) override { v->visit(this); }
};

class TypeSlice final : public TypeNext
{
public:
    Expression *lwr;
    Expression *upr;

    const char *kind() override;
    TypeSlice *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class TypeNull final : public Type
{
public:
    const char *kind() override;

    TypeNull *syntaxCopy() override;
    MATCH implicitConvTo(Type *to) override;
    bool isBoolean() override;

    uinteger_t size(const Loc &loc) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class TypeNoreturn final : public Type
{
public:
    const char *kind() override;
    TypeNoreturn *syntaxCopy() override;
    MATCH implicitConvTo(Type* to) override;
    MATCH constConv(Type* to) override;
    bool isBoolean() override;
    uinteger_t size(const Loc& loc) override;
    unsigned alignsize() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeTag final : public Type
{
public:
    TypeTag *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

/**************************************************************/

namespace dmd
{
    // If the type is a class or struct, returns the symbol for it, else null.
    AggregateDeclaration *isAggregate(Type *t);
    bool hasPointers(Type *t);
    // return the symbol to which type t resolves
    Dsymbol *toDsymbol(Type *t, Scope *sc);
    bool equivalent(Type *src, Type *t);
    Covariant covariant(Type *, Type *, StorageClass * = NULL, bool = false);
    bool isBaseOf(Type *tthis, Type *t, int *poffset);
    Type *trySemantic(Type *type, const Loc &loc, Scope *sc);
    Type *pointerTo(Type *type);
    Type *referenceTo(Type *type);
    Type *merge2(Type *type);
    Type *sarrayOf(Type *type, dinteger_t dim);
    Type *arrayOf(Type *type);
    Type *constOf(Type *type);
    Type *immutableOf(Type *type);
    Type *mutableOf(Type *type);
    Type *sharedOf(Type *type);
    Type *sharedConstOf(Type *type);
    Type *unSharedOf(Type *type);
    Type *wildOf(Type *type);
    Type *wildConstOf(Type *type);
    Type *sharedWildOf(Type *type);
    Type *sharedWildConstOf(Type *type);
    Type *unqualify(Type *type, unsigned m);
    Type *toHeadMutable(Type *type);
    Type *aliasthisOf(Type *type);
    Type *castMod(Type *type, MOD mod);
    Type *addMod(Type *type, MOD mod);
    Type *addStorageClass(Type *type, StorageClass stc);
    Type *substWildTo(Type *type, unsigned mod);
}
