
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/expression.h
 */

#pragma once

#include "ast_node.h"
#include "globals.h"
#include "arraytypes.h"
#include "visitor.h"
#include "tokens.h"

#include "root/complex_t.h"
#include "root/dcompat.h"
#include "root/optional.h"

class Type;
class TypeVector;
struct Scope;
class TupleDeclaration;
class VarDeclaration;
class FuncDeclaration;
class FuncLiteralDeclaration;
class CtorDeclaration;
class Dsymbol;
class ScopeDsymbol;
class Expression;
class Declaration;
class StructDeclaration;
class TemplateInstance;
class TemplateDeclaration;
class ClassDeclaration;
class OverloadSet;
class StringExp;
struct UnionExp;
#ifdef IN_GCC
typedef union tree_node Symbol;
#else
struct Symbol;          // back end symbol
#endif

void expandTuples(Expressions *exps, Identifiers *names = nullptr);
bool isTrivialExp(Expression *e);
bool hasSideEffect(Expression *e, bool assumeImpureCalls = false);

enum BE : int32_t;
BE canThrow(Expression *e, FuncDeclaration *func, bool mustNotThrow);

typedef unsigned char OwnedBy;
enum
{
    OWNEDcode,      // normal code expression in AST
    OWNEDctfe,      // value expression for CTFE
    OWNEDcache      // constant value cached for CTFE
};

#define WANTvalue  0 // default
#define WANTexpand 1 // expand const/immutable variables if possible

/**
 * Specifies how the checkModify deals with certain situations
 */
enum class ModifyFlags
{
    /// Issue error messages on invalid modifications of the variable
    none,
    /// No errors are emitted for invalid modifications
    noError = 0x1,
    /// The modification occurs for a subfield of the current variable
    fieldAssign = 0x2,
};

class Expression : public ASTNode
{
public:
    EXP op;                     // to minimize use of dynamic_cast
    unsigned char size;         // # of bytes in Expression so we can copy() it
    d_bool parens;                // if this is a parenthesized expression
    Type *type;                 // !=NULL means that semantic() has been run
    Loc loc;                    // file location

    static void _init();
    Expression *copy();
    virtual Expression *syntaxCopy();

    // kludge for template.isExpression()
    DYNCAST dyncast() const override final { return DYNCAST_EXPRESSION; }

    const char *toChars() const override;
    void error(const char *format, ...) const;
    void warning(const char *format, ...) const;
    void deprecation(const char *format, ...) const;

    virtual dinteger_t toInteger();
    virtual uinteger_t toUInteger();
    virtual real_t toReal();
    virtual real_t toImaginary();
    virtual complex_t toComplex();
    virtual StringExp *toStringExp();
    virtual bool isLvalue();
    virtual Expression *toLvalue(Scope *sc, Expression *e);
    virtual Expression *modifiableLvalue(Scope *sc, Expression *e);
    Expression *implicitCastTo(Scope *sc, Type *t);
    MATCH implicitConvTo(Type *t);
    Expression *castTo(Scope *sc, Type *t);
    virtual Expression *resolveLoc(const Loc &loc, Scope *sc);
    virtual bool checkType();
    virtual bool checkValue();
    bool checkDeprecated(Scope *sc, Dsymbol *s);
    virtual Expression *addDtorHook(Scope *sc);
    Expression *addressOf();
    Expression *deref();

    Expression *optimize(int result, bool keepLvalue = false);

    // Entry point for CTFE.
    // A compile-time result is required. Give an error if not possible
    Expression *ctfeInterpret();
    int isConst();
    virtual bool isIdentical(const Expression *e) const;
    virtual Optional<bool> toBool();
    virtual bool hasCode()
    {
        return true;
    }

    IntegerExp* isIntegerExp();
    ErrorExp* isErrorExp();
    VoidInitExp* isVoidInitExp();
    RealExp* isRealExp();
    ComplexExp* isComplexExp();
    IdentifierExp* isIdentifierExp();
    DollarExp* isDollarExp();
    DsymbolExp* isDsymbolExp();
    ThisExp* isThisExp();
    SuperExp* isSuperExp();
    NullExp* isNullExp();
    StringExp* isStringExp();
    TupleExp* isTupleExp();
    ArrayLiteralExp* isArrayLiteralExp();
    AssocArrayLiteralExp* isAssocArrayLiteralExp();
    StructLiteralExp* isStructLiteralExp();
    TypeExp* isTypeExp();
    ScopeExp* isScopeExp();
    TemplateExp* isTemplateExp();
    NewExp* isNewExp();
    NewAnonClassExp* isNewAnonClassExp();
    SymOffExp* isSymOffExp();
    VarExp* isVarExp();
    OverExp* isOverExp();
    FuncExp* isFuncExp();
    DeclarationExp* isDeclarationExp();
    TypeidExp* isTypeidExp();
    TraitsExp* isTraitsExp();
    HaltExp* isHaltExp();
    IsExp* isExp();
    MixinExp* isMixinExp();
    ImportExp* isImportExp();
    AssertExp* isAssertExp();
    DotIdExp* isDotIdExp();
    DotTemplateExp* isDotTemplateExp();
    DotVarExp* isDotVarExp();
    DotTemplateInstanceExp* isDotTemplateInstanceExp();
    DelegateExp* isDelegateExp();
    DotTypeExp* isDotTypeExp();
    CallExp* isCallExp();
    AddrExp* isAddrExp();
    PtrExp* isPtrExp();
    NegExp* isNegExp();
    UAddExp* isUAddExp();
    ComExp* isComExp();
    NotExp* isNotExp();
    DeleteExp* isDeleteExp();
    CastExp* isCastExp();
    VectorExp* isVectorExp();
    VectorArrayExp* isVectorArrayExp();
    SliceExp* isSliceExp();
    ArrayLengthExp* isArrayLengthExp();
    ArrayExp* isArrayExp();
    DotExp* isDotExp();
    CommaExp* isCommaExp();
    IntervalExp* isIntervalExp();
    DelegatePtrExp* isDelegatePtrExp();
    DelegateFuncptrExp* isDelegateFuncptrExp();
    IndexExp* isIndexExp();
    PostExp* isPostExp();
    PreExp* isPreExp();
    AssignExp* isAssignExp();
    ConstructExp* isConstructExp();
    BlitExp* isBlitExp();
    AddAssignExp* isAddAssignExp();
    MinAssignExp* isMinAssignExp();
    MulAssignExp* isMulAssignExp();
    DivAssignExp* isDivAssignExp();
    ModAssignExp* isModAssignExp();
    AndAssignExp* isAndAssignExp();
    OrAssignExp* isOrAssignExp();
    XorAssignExp* isXorAssignExp();
    PowAssignExp* isPowAssignExp();
    ShlAssignExp* isShlAssignExp();
    ShrAssignExp* isShrAssignExp();
    UshrAssignExp* isUshrAssignExp();
    CatAssignExp* isCatAssignExp();
    CatElemAssignExp* isCatElemAssignExp();
    CatDcharAssignExp* isCatDcharAssignExp();
    AddExp* isAddExp();
    MinExp* isMinExp();
    CatExp* isCatExp();
    MulExp* isMulExp();
    DivExp* isDivExp();
    ModExp* isModExp();
    PowExp* isPowExp();
    ShlExp* isShlExp();
    ShrExp* isShrExp();
    UshrExp* isUshrExp();
    AndExp* isAndExp();
    OrExp* isOrExp();
    XorExp* isXorExp();
    LogicalExp* isLogicalExp();
    InExp* isInExp();
    RemoveExp* isRemoveExp();
    EqualExp* isEqualExp();
    IdentityExp* isIdentityExp();
    CondExp* isCondExp();
    GenericExp* isGenericExp();
    DefaultInitExp* isDefaultInitExp();
    FileInitExp* isFileInitExp();
    LineInitExp* isLineInitExp();
    ModuleInitExp* isModuleInitExp();
    FuncInitExp* isFuncInitExp();
    PrettyFuncInitExp* isPrettyFuncInitExp();
    ClassReferenceExp* isClassReferenceExp();
    ThrownExceptionExp* isThrownExceptionExp();
    UnaExp* isUnaExp();
    BinExp* isBinExp();
    BinAssignExp* isBinAssignExp();

    void accept(Visitor *v) override { v->visit(this); }
};

class IntegerExp final : public Expression
{
public:
    dinteger_t value;

    static IntegerExp *create(const Loc &loc, dinteger_t value, Type *type);
    static void emplace(UnionExp *pue, const Loc &loc, dinteger_t value, Type *type);
    bool equals(const RootObject * const o) const override;
    dinteger_t toInteger() override;
    real_t toReal() override;
    real_t toImaginary() override;
    complex_t toComplex() override;
    Optional<bool> toBool() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    void accept(Visitor *v) override { v->visit(this); }
    dinteger_t getInteger() { return value; }
    void setInteger(dinteger_t value);
    template<int v>
    static IntegerExp literal();
};

class ErrorExp final : public Expression
{
public:
    Expression *toLvalue(Scope *sc, Expression *e) override;
    void accept(Visitor *v) override { v->visit(this); }

    static ErrorExp *errorexp; // handy shared value
};

class RealExp final : public Expression
{
public:
    real_t value;

    static RealExp *create(const Loc &loc, real_t value, Type *type);
    static void emplace(UnionExp *pue, const Loc &loc, real_t value, Type *type);
    bool equals(const RootObject * const o) const override;
    bool isIdentical(const Expression *e) const override;
    dinteger_t toInteger() override;
    uinteger_t toUInteger() override;
    real_t toReal() override;
    real_t toImaginary() override;
    complex_t toComplex() override;
    Optional<bool> toBool() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class ComplexExp final : public Expression
{
public:
    complex_t value;

    static ComplexExp *create(const Loc &loc, complex_t value, Type *type);
    static void emplace(UnionExp *pue, const Loc &loc, complex_t value, Type *type);
    bool equals(const RootObject * const o) const override;
    bool isIdentical(const Expression *e) const override;
    dinteger_t toInteger() override;
    uinteger_t toUInteger() override;
    real_t toReal() override;
    real_t toImaginary() override;
    complex_t toComplex() override;
    Optional<bool> toBool() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class IdentifierExp : public Expression
{
public:
    Identifier *ident;

    static IdentifierExp *create(const Loc &loc, Identifier *ident);
    bool isLvalue() override final;
    Expression *toLvalue(Scope *sc, Expression *e) override final;
    void accept(Visitor *v) override { v->visit(this); }
};

class DollarExp final : public IdentifierExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class DsymbolExp final : public Expression
{
public:
    Dsymbol *s;
    d_bool hasOverloads;

    DsymbolExp *syntaxCopy() override;
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class ThisExp : public Expression
{
public:
    VarDeclaration *var;

    ThisExp *syntaxCopy() override;
    Optional<bool> toBool() override;
    bool isLvalue() override final;
    Expression *toLvalue(Scope *sc, Expression *e) override final;

    void accept(Visitor *v) override { v->visit(this); }
};

class SuperExp final : public ThisExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class NullExp final : public Expression
{
public:
    bool equals(const RootObject * const o) const override;
    Optional<bool> toBool() override;
    StringExp *toStringExp() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class StringExp final : public Expression
{
public:
    void *string;       // char, wchar, or dchar data
    size_t len;         // number of chars, wchars, or dchars
    unsigned char sz;   // 1: char, 2: wchar, 4: dchar
    unsigned char committed;    // !=0 if type is committed
    utf8_t postfix;      // 'c', 'w', 'd'
    OwnedBy ownedByCtfe;

    static StringExp *create(const Loc &loc, const char *s);
    static StringExp *create(const Loc &loc, const void *s, d_size_t len);
    static void emplace(UnionExp *pue, const Loc &loc, const char *s);
    bool equals(const RootObject * const o) const override;
    char32_t getCodeUnit(d_size_t i) const;
    void setCodeUnit(d_size_t i, char32_t c);
    StringExp *toStringExp() override;
    StringExp *toUTF8(Scope *sc);
    Optional<bool> toBool() override;
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    Expression *modifiableLvalue(Scope *sc, Expression *e) override;
    void accept(Visitor *v) override { v->visit(this); }
    size_t numberOfCodeUnits(int tynto = 0) const;
    void writeTo(void* dest, bool zero, int tyto = 0) const;
};

// Tuple

class TupleExp final : public Expression
{
public:
    Expression *e0;     // side-effect part
    /* Tuple-field access may need to take out its side effect part.
     * For example:
     *      foo().tupleof
     * is rewritten as:
     *      (ref __tup = foo(); tuple(__tup.field0, __tup.field1, ...))
     * The declaration of temporary variable __tup will be stored in TupleExp::e0.
     */
    Expressions *exps;

    static TupleExp *create(const Loc &loc, Expressions *exps);
    TupleExp *syntaxCopy() override;
    bool equals(const RootObject * const o) const override;

    void accept(Visitor *v) override { v->visit(this); }
};

class ArrayLiteralExp final : public Expression
{
public:
    Expression *basis;
    Expressions *elements;
    OwnedBy ownedByCtfe;
    d_bool onstack;

    static ArrayLiteralExp *create(const Loc &loc, Expressions *elements);
    static void emplace(UnionExp *pue, const Loc &loc, Expressions *elements);
    ArrayLiteralExp *syntaxCopy() override;
    bool equals(const RootObject * const o) const override;
    Expression *getElement(d_size_t i); // use opIndex instead
    Expression *opIndex(d_size_t i);
    Optional<bool> toBool() override;
    StringExp *toStringExp() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class AssocArrayLiteralExp final : public Expression
{
public:
    Expressions *keys;
    Expressions *values;
    OwnedBy ownedByCtfe;

    bool equals(const RootObject * const o) const override;
    AssocArrayLiteralExp *syntaxCopy() override;
    Optional<bool> toBool() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class StructLiteralExp final : public Expression
{
public:
    StructDeclaration *sd;      // which aggregate this is for
    Expressions *elements;      // parallels sd->fields[] with NULL entries for fields to skip
    Type *stype;                // final type of result (can be different from sd's type)

    Symbol *sym;                // back end symbol to initialize with literal

    /** pointer to the origin instance of the expression.
     * once a new expression is created, origin is set to 'this'.
     * anytime when an expression copy is created, 'origin' pointer is set to
     * 'origin' pointer value of the original expression.
     */
    StructLiteralExp *origin;

    // those fields need to prevent a infinite recursion when one field of struct initialized with 'this' pointer.
    StructLiteralExp *inlinecopy;

    /** anytime when recursive function is calling, 'stageflags' marks with bit flag of
     * current stage and unmarks before return from this function.
     * 'inlinecopy' uses similar 'stageflags' and from multiple evaluation 'doInline'
     * (with infinite recursion) of this expression.
     */
    int stageflags;

    d_bool useStaticInit;         // if this is true, use the StructDeclaration's init symbol
    d_bool isOriginal;            // used when moving instances to indicate `this is this.origin`
    OwnedBy ownedByCtfe;

    static StructLiteralExp *create(const Loc &loc, StructDeclaration *sd, void *elements, Type *stype = NULL);
    bool equals(const RootObject * const o) const override;
    StructLiteralExp *syntaxCopy() override;
    Expression *getField(Type *type, unsigned offset);
    int getFieldIndex(Type *type, unsigned offset);
    Expression *addDtorHook(Scope *sc) override;
    Expression *toLvalue(Scope *sc, Expression *e) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeExp final : public Expression
{
public:
    TypeExp *syntaxCopy() override;
    bool checkType() override;
    bool checkValue() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class ScopeExp final : public Expression
{
public:
    ScopeDsymbol *sds;

    ScopeExp *syntaxCopy() override;
    bool checkType() override;
    bool checkValue() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class TemplateExp final : public Expression
{
public:
    TemplateDeclaration *td;
    FuncDeclaration *fd;

    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    bool checkType() override;
    bool checkValue() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class NewExp final : public Expression
{
public:
    /* newtype(arguments)
     */
    Expression *thisexp;        // if !NULL, 'this' for class being allocated
    Type *newtype;
    Expressions *arguments;     // Array of Expression's
    Identifiers *names;         // Array of names corresponding to expressions

    Expression *argprefix;      // expression to be evaluated just before arguments[]

    CtorDeclaration *member;    // constructor function
    d_bool onstack;               // allocate on stack
    d_bool thrownew;              // this NewExp is the expression of a ThrowStatement

    Expression *lowering;       // lowered druntime hook: `_d_newclass`

    static NewExp *create(const Loc &loc, Expression *thisexp, Type *newtype, Expressions *arguments);
    NewExp *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class NewAnonClassExp final : public Expression
{
public:
    /* class baseclasses { } (arguments)
     */
    Expression *thisexp;        // if !NULL, 'this' for class being allocated
    ClassDeclaration *cd;       // class being instantiated
    Expressions *arguments;     // Array of Expression's to call class constructor

    NewAnonClassExp *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class SymbolExp : public Expression
{
public:
    Declaration *var;
    Dsymbol *originalScope;
    d_bool hasOverloads;

    void accept(Visitor *v) override { v->visit(this); }
};

// Offset from symbol

class SymOffExp final : public SymbolExp
{
public:
    dinteger_t offset;

    Optional<bool> toBool() override;

    void accept(Visitor *v) override { v->visit(this); }
};

// Variable

class VarExp final : public SymbolExp
{
public:
    d_bool delegateWasExtracted;
    static VarExp *create(const Loc &loc, Declaration *var, bool hasOverloads = true);
    bool equals(const RootObject * const o) const override;
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    Expression *modifiableLvalue(Scope *sc, Expression *e) override;

    void accept(Visitor *v) override { v->visit(this); }
};

// Overload Set

class OverExp final : public Expression
{
public:
    OverloadSet *vars;

    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    void accept(Visitor *v) override { v->visit(this); }
};

// Function/Delegate literal

class FuncExp final : public Expression
{
public:
    FuncLiteralDeclaration *fd;
    TemplateDeclaration *td;
    TOK tok;

    bool equals(const RootObject * const o) const override;
    FuncExp *syntaxCopy() override;
    const char *toChars() const override;
    bool checkType() override;
    bool checkValue() override;

    void accept(Visitor *v) override { v->visit(this); }
};

// Declaration of a symbol

// D grammar allows declarations only as statements. However in AST representation
// it can be part of any expression. This is used, for example, during internal
// syntax re-writes to inject hidden symbols.
class DeclarationExp final : public Expression
{
public:
    Dsymbol *declaration;

    DeclarationExp *syntaxCopy() override;

    bool hasCode() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class TypeidExp final : public Expression
{
public:
    RootObject *obj;

    TypeidExp *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class TraitsExp final : public Expression
{
public:
    Identifier *ident;
    Objects *args;

    TraitsExp *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class HaltExp final : public Expression
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class IsExp final : public Expression
{
public:
    /* is(targ id tok tspec)
     * is(targ id == tok2)
     */
    Type *targ;
    Identifier *id;     // can be NULL
    Type *tspec;        // can be NULL
    TemplateParameters *parameters;
    TOK tok;       // ':' or '=='
    TOK tok2;      // 'struct', 'union', etc.

    IsExp *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

/****************************************************************/

class UnaExp : public Expression
{
public:
    Expression *e1;
    Type *att1; // Save alias this type to detect recursion

    UnaExp *syntaxCopy() override;
    Expression *incompatibleTypes();
    Expression *resolveLoc(const Loc &loc, Scope *sc) override final;

    void accept(Visitor *v) override { v->visit(this); }
};

class BinExp : public Expression
{
public:
    Expression *e1;
    Expression *e2;

    Type *att1; // Save alias this type to detect recursion
    Type *att2; // Save alias this type to detect recursion

    BinExp *syntaxCopy() override;
    Expression *incompatibleTypes();

    Expression *reorderSettingAAElem(Scope *sc);

    void accept(Visitor *v) override { v->visit(this); }
};

class BinAssignExp : public BinExp
{
public:
    bool isLvalue() override final;
    Expression *toLvalue(Scope *sc, Expression *ex) override final;
    Expression *modifiableLvalue(Scope *sc, Expression *e) override final;
    void accept(Visitor *v) override { v->visit(this); }
};

/****************************************************************/

class MixinExp final : public UnaExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class ImportExp final : public UnaExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class AssertExp final : public UnaExp
{
public:
    Expression *msg;

    AssertExp *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class ThrowExp final : public UnaExp
{
public:
    ThrowExp *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class DotIdExp final : public UnaExp
{
public:
    Identifier *ident;
    d_bool noderef;       // true if the result of the expression will never be dereferenced
    d_bool wantsym;       // do not replace Symbol with its initializer during semantic()
    d_bool arrow;         // ImportC: if -> instead of .

    static DotIdExp *create(const Loc &loc, Expression *e, Identifier *ident);
    void accept(Visitor *v) override { v->visit(this); }
};

class DotTemplateExp final : public UnaExp
{
public:
    TemplateDeclaration *td;

    bool checkType() override;
    bool checkValue() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class DotVarExp final : public UnaExp
{
public:
    Declaration *var;
    d_bool hasOverloads;

    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    Expression *modifiableLvalue(Scope *sc, Expression *e) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class DotTemplateInstanceExp final : public UnaExp
{
public:
    TemplateInstance *ti;

    DotTemplateInstanceExp *syntaxCopy() override;
    bool findTempDecl(Scope *sc);
    bool checkType() override;
    bool checkValue() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class DelegateExp final : public UnaExp
{
public:
    FuncDeclaration *func;
    d_bool hasOverloads;
    VarDeclaration *vthis2;  // container for multi-context


    void accept(Visitor *v) override { v->visit(this); }
};

class DotTypeExp final : public UnaExp
{
public:
    Dsymbol *sym;               // symbol that represents a type

    void accept(Visitor *v) override { v->visit(this); }
};

class CallExp final : public UnaExp
{
public:
    Expressions *arguments;     // function arguments
    Identifiers *names;
    FuncDeclaration *f;         // symbol to call
    d_bool directcall;            // true if a virtual call is devirtualized
    d_bool inDebugStatement;      // true if this was in a debug statement
    d_bool ignoreAttributes;      // don't enforce attributes (e.g. call @gc function in @nogc code)
    VarDeclaration *vthis2;     // container for multi-context

    static CallExp *create(const Loc &loc, Expression *e, Expressions *exps);
    static CallExp *create(const Loc &loc, Expression *e);
    static CallExp *create(const Loc &loc, Expression *e, Expression *earg1);
    static CallExp *create(const Loc &loc, FuncDeclaration *fd, Expression *earg1);

    CallExp *syntaxCopy() override;
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    Expression *addDtorHook(Scope *sc) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class AddrExp final : public UnaExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class PtrExp final : public UnaExp
{
public:
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    Expression *modifiableLvalue(Scope *sc, Expression *e) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class NegExp final : public UnaExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class UAddExp final : public UnaExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class ComExp final : public UnaExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class NotExp final : public UnaExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class DeleteExp final : public UnaExp
{
public:
    d_bool isRAII;
    void accept(Visitor *v) override { v->visit(this); }
};

class CastExp final : public UnaExp
{
public:
    // Possible to cast to one type while painting to another type
    Type *to;                   // type to cast to
    unsigned char mod;          // MODxxxxx

    CastExp *syntaxCopy() override;
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class VectorExp final : public UnaExp
{
public:
    TypeVector *to;             // the target vector type before semantic()
    unsigned dim;               // number of elements in the vector
    OwnedBy ownedByCtfe;

    static VectorExp *create(const Loc &loc, Expression *e, Type *t);
    static void emplace(UnionExp *pue, const Loc &loc, Expression *e, Type *t);
    VectorExp *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class VectorArrayExp final : public UnaExp
{
public:
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class SliceExp final : public UnaExp
{
public:
    Expression *upr;            // NULL if implicit 0
    Expression *lwr;            // NULL if implicit [length - 1]
    VarDeclaration *lengthVar;
    d_bool upperIsInBounds;       // true if upr <= e1.length
    d_bool lowerIsLessThanUpper;  // true if lwr <= upr
    d_bool arrayop;               // an array operation, rather than a slice

    SliceExp *syntaxCopy() override;
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    Expression *modifiableLvalue(Scope *sc, Expression *e) override;
    Optional<bool> toBool() override;

    void accept(Visitor *v) override { v->visit(this); }
};

class ArrayLengthExp final : public UnaExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class IntervalExp final : public Expression
{
public:
    Expression *lwr;
    Expression *upr;

    IntervalExp *syntaxCopy() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class DelegatePtrExp final : public UnaExp
{
public:
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    Expression *modifiableLvalue(Scope *sc, Expression *e) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class DelegateFuncptrExp final : public UnaExp
{
public:
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    Expression *modifiableLvalue(Scope *sc, Expression *e) override;
    void accept(Visitor *v) override { v->visit(this); }
};

// e1[a0,a1,a2,a3,...]

class ArrayExp final : public UnaExp
{
public:
    Expressions *arguments;             // Array of Expression's
    size_t currentDimension;            // for opDollar
    VarDeclaration *lengthVar;

    ArrayExp *syntaxCopy() override;
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;

    void accept(Visitor *v) override { v->visit(this); }
};

/****************************************************************/

class DotExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class CommaExp final : public BinExp
{
public:
    d_bool isGenerated;
    d_bool allowCommaExp;
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    Expression *modifiableLvalue(Scope *sc, Expression *e) override;
    Optional<bool> toBool() override;
    Expression *addDtorHook(Scope *sc) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class IndexExp final : public BinExp
{
public:
    VarDeclaration *lengthVar;
    d_bool modifiable;
    d_bool indexIsInBounds;       // true if 0 <= e2 && e2 <= e1.length - 1

    IndexExp *syntaxCopy() override;
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    Expression *modifiableLvalue(Scope *sc, Expression *e) override;

    void accept(Visitor *v) override { v->visit(this); }
};

/* For both i++ and i--
 */
class PostExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

/* For both ++i and --i
 */
class PreExp final : public UnaExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

enum class MemorySet
{
    none            = 0,    // simple assignment
    blockAssign     = 1,    // setting the contents of an array
    referenceInit   = 2     // setting the reference of STCref variable
};

class AssignExp : public BinExp
{
public:
    MemorySet memset;

    bool isLvalue() override final;
    Expression *toLvalue(Scope *sc, Expression *ex) override final;

    void accept(Visitor *v) override { v->visit(this); }
};

class ConstructExp final : public AssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class BlitExp final : public AssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class AddAssignExp final : public BinAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class MinAssignExp final : public BinAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class MulAssignExp final : public BinAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class DivAssignExp final : public BinAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class ModAssignExp final : public BinAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class AndAssignExp final : public BinAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class OrAssignExp final : public BinAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class XorAssignExp final : public BinAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class PowAssignExp final : public BinAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class ShlAssignExp final : public BinAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class ShrAssignExp final : public BinAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class UshrAssignExp final : public BinAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class CatAssignExp : public BinAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class CatElemAssignExp final : public CatAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class CatDcharAssignExp final : public CatAssignExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class AddExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class MinExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class CatExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class MulExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class DivExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class ModExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class PowExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class ShlExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class ShrExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class UshrExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class AndExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class OrExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class XorExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class LogicalExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class CmpExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class InExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class RemoveExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

// == and !=

class EqualExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

// is and !is

class IdentityExp final : public BinExp
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

/****************************************************************/

class CondExp final : public BinExp
{
public:
    Expression *econd;

    CondExp *syntaxCopy() override;
    bool isLvalue() override;
    Expression *toLvalue(Scope *sc, Expression *e) override;
    Expression *modifiableLvalue(Scope *sc, Expression *e) override;
    void hookDtors(Scope *sc);

    void accept(Visitor *v) override { v->visit(this); }
};

class GenericExp final : Expression
{
    Expression *cntlExp;
    Types *types;
    Expressions *exps;

    GenericExp *syntaxCopy() override;

    void accept(Visitor *v) override { v->visit(this); }
};

/****************************************************************/

class DefaultInitExp : public Expression
{
public:
    void accept(Visitor *v) override { v->visit(this); }
};

class FileInitExp final : public DefaultInitExp
{
public:
    Expression *resolveLoc(const Loc &loc, Scope *sc) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class LineInitExp final : public DefaultInitExp
{
public:
    Expression *resolveLoc(const Loc &loc, Scope *sc) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class ModuleInitExp final : public DefaultInitExp
{
public:
    Expression *resolveLoc(const Loc &loc, Scope *sc) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class FuncInitExp final : public DefaultInitExp
{
public:
    Expression *resolveLoc(const Loc &loc, Scope *sc) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class PrettyFuncInitExp final : public DefaultInitExp
{
public:
    Expression *resolveLoc(const Loc &loc, Scope *sc) override;
    void accept(Visitor *v) override { v->visit(this); }
};

/****************************************************************/

/* A type meant as a union of all the Expression types,
 * to serve essentially as a Variant that will sit on the stack
 * during CTFE to reduce memory consumption.
 */
struct UnionExp
{
    UnionExp() { }  // yes, default constructor does nothing

    UnionExp(Expression *e)
    {
        memcpy(this, (void *)e, e->size);
    }

    /* Extract pointer to Expression
     */
    Expression *exp() { return (Expression *)&u; }

    /* Convert to an allocated Expression
     */
    Expression *copy();

private:
    // Ensure that the union is suitably aligned.
#if defined(__GNUC__) || defined(__clang__)
    __attribute__((aligned(8)))
#elif defined(_MSC_VER)
    __declspec(align(8))
#elif defined(__DMC__)
    #pragma pack(8)
#endif
    union
    {
        char exp       [sizeof(Expression)];
        char integerexp[sizeof(IntegerExp)];
        char errorexp  [sizeof(ErrorExp)];
        char realexp   [sizeof(RealExp)];
        char complexexp[sizeof(ComplexExp)];
        char symoffexp [sizeof(SymOffExp)];
        char stringexp [sizeof(StringExp)];
        char arrayliteralexp [sizeof(ArrayLiteralExp)];
        char assocarrayliteralexp [sizeof(AssocArrayLiteralExp)];
        char structliteralexp [sizeof(StructLiteralExp)];
        char nullexp   [sizeof(NullExp)];
        char dotvarexp [sizeof(DotVarExp)];
        char addrexp   [sizeof(AddrExp)];
        char indexexp  [sizeof(IndexExp)];
        char sliceexp  [sizeof(SliceExp)];
        char vectorexp [sizeof(VectorExp)];
    } u;
#if defined(__DMC__)
    #pragma pack()
#endif
};

/****************************************************************/

class ObjcClassReferenceExp final : public Expression
{
public:
    ClassDeclaration* classDeclaration;

    void accept(Visitor *v) override { v->visit(this); }
};
