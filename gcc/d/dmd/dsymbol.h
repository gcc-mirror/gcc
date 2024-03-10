
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/dsymbol.h
 */

#pragma once

#include "root/port.h"
#include "ast_node.h"
#include "globals.h"
#include "arraytypes.h"
#include "visitor.h"

class CPPNamespaceDeclaration;
class Identifier;
struct Scope;
class DsymbolTable;
class Declaration;
class ThisDeclaration;
class BitFieldDeclaration;
class TypeInfoDeclaration;
class TupleDeclaration;
class AliasDeclaration;
class AggregateDeclaration;
class EnumDeclaration;
class ClassDeclaration;
class InterfaceDeclaration;
class StructDeclaration;
class UnionDeclaration;
class FuncDeclaration;
class FuncAliasDeclaration;
class OverDeclaration;
class FuncLiteralDeclaration;
class CtorDeclaration;
class PostBlitDeclaration;
class DtorDeclaration;
class StaticCtorDeclaration;
class StaticDtorDeclaration;
class SharedStaticCtorDeclaration;
class SharedStaticDtorDeclaration;
class InvariantDeclaration;
class UnitTestDeclaration;
class NewDeclaration;
class VarDeclaration;
class AttribDeclaration;
class VisibilityDeclaration;
class Package;
class Module;
class Import;
class Type;
class TypeTuple;
class WithStatement;
class LabelDsymbol;
class ScopeDsymbol;
class ForwardingScopeDsymbol;
class TemplateDeclaration;
class TemplateInstance;
class TemplateMixin;
class ForwardingAttribDeclaration;
class Nspace;
class EnumMember;
class WithScopeSymbol;
class ArrayScopeSymbol;
class SymbolDeclaration;
class Expression;
class ExpressionDsymbol;
class AliasAssign;
class OverloadSet;
class StaticAssert;
class StaticIfDeclaration;
struct AA;
#ifdef IN_GCC
typedef union tree_node Symbol;
#else
struct Symbol;
#endif

struct Ungag
{
    unsigned oldgag;

    Ungag(unsigned old) : oldgag(old) {}
    ~Ungag() { global.gag = oldgag; }
};

enum class ThreeState : uint8_t
{
    none,         // value not yet computed
    no,           // value is false
    yes,          // value is true
};

void dsymbolSemantic(Dsymbol *dsym, Scope *sc);
void semantic2(Dsymbol *dsym, Scope *sc);
void semantic3(Dsymbol *dsym, Scope* sc);

struct Visibility
{
    enum Kind
    {
        undefined,
        none,           // no access
        private_,
        package_,
        protected_,
        public_,
        export_
    };
    Kind kind;
    Package *pkg;
};

/* State of symbol in winding its way through the passes of the compiler
 */
enum class PASS : uint8_t
{
    initial,        // initial state
    semantic,       // semantic() started
    semanticdone,   // semantic() done
    semantic2,      // semantic2() started
    semantic2done,  // semantic2() done
    semantic3,      // semantic3() started
    semantic3done,  // semantic3() done
    inline_,         // inline started
    inlinedone,     // inline done
    obj             // toObjFile() run
};

enum
{
    PASSinit,           // initial state
    PASSsemantic,       // semantic() started
    PASSsemanticdone,   // semantic() done
    PASSsemantic2,      // semantic2() started
    PASSsemantic2done,  // semantic2() done
    PASSsemantic3,      // semantic3() started
    PASSsemantic3done,  // semantic3() done
    PASSinline,         // inline started
    PASSinlinedone,     // inline done
    PASSobj             // toObjFile() run
};

/* Flags for symbol search
 */
enum
{
    IgnoreNone              = 0x00, // default
    IgnorePrivateImports    = 0x01, // don't search private imports
    IgnoreErrors            = 0x02, // don't give error messages
    IgnoreAmbiguous         = 0x04, // return NULL if ambiguous
    SearchLocalsOnly        = 0x08, // only look at locals (don't search imports)
    SearchImportsOnly       = 0x10, // only look in imports
    SearchUnqualifiedModule = 0x20, // the module scope search is unqualified,
                                    // meaning don't search imports in that scope,
                                    // because qualified module searches search
                                    // their imports
    IgnoreSymbolVisibility  = 0x80,  // also find private and package protected symbols
    TagNameSpace            = 0x100, // search ImportC tag symbol table
};

struct FieldState
{
    unsigned offset;

    unsigned fieldOffset;
    unsigned fieldSize;
    unsigned fieldAlign;
    unsigned bitOffset;

    d_bool inFlight;
};

struct DsymbolAttributes;

class Dsymbol : public ASTNode
{
public:
    Identifier *ident;
    Dsymbol *parent;
    Symbol *csym;               // symbol for code generator
    Loc loc;                    // where defined
    Scope *_scope;               // !=NULL means context to use for semantic()
    const utf8_t *prettystring;
private:
    DsymbolAttributes* atts;
public:
    d_bool errors;                // this symbol failed to pass semantic()
    PASS semanticRun;
    unsigned short localNum;        // perturb mangled name to avoid collisions with those in FuncDeclaration.localsymtab
    static Dsymbol *create(Identifier *);
    const char *toChars() const override;
    DeprecatedDeclaration* depdecl();
    CPPNamespaceDeclaration* cppnamespace();
    UserAttributeDeclaration* userAttribDecl();
    DeprecatedDeclaration* depdecl(DeprecatedDeclaration* dd);
    CPPNamespaceDeclaration* cppnamespace(CPPNamespaceDeclaration* ns);
    UserAttributeDeclaration* userAttribDecl(UserAttributeDeclaration* uad);
    virtual const char *toPrettyCharsHelper(); // helper to print fully qualified (template) arguments
    Loc getLoc();
    const char *locToChars();
    bool equals(const RootObject * const o) const override;
    bool isAnonymous() const;
    void error(const Loc &loc, const char *format, ...);
    void error(const char *format, ...);
    void deprecation(const Loc &loc, const char *format, ...);
    void deprecation(const char *format, ...);
    bool checkDeprecated(const Loc &loc, Scope *sc);
    Module *getModule();
    bool isCsymbol();
    Module *getAccessModule();
    Dsymbol *pastMixin();
    Dsymbol *toParent();
    Dsymbol *toParent2();
    Dsymbol *toParentDecl();
    Dsymbol *toParentLocal();
    Dsymbol *toParentP(Dsymbol *p1, Dsymbol *p2 = NULL);
    TemplateInstance *isInstantiated();
    bool followInstantiationContext(Dsymbol *p1, Dsymbol *p2 = NULL);
    TemplateInstance *isSpeculative();
    Ungag ungagSpeculative();

    // kludge for template.isSymbol()
    DYNCAST dyncast() const override final { return DYNCAST_DSYMBOL; }

    virtual Identifier *getIdent();
    virtual const char *toPrettyChars(bool QualifyTypes = false);
    virtual const char *kind() const;
    virtual Dsymbol *toAlias();                 // resolve real symbol
    virtual Dsymbol *toAlias2();
    virtual void addMember(Scope *sc, ScopeDsymbol *sds);
    virtual void setScope(Scope *sc);
    virtual void importAll(Scope *sc);
    virtual Dsymbol *search(const Loc &loc, Identifier *ident, int flags = IgnoreNone);
    virtual bool overloadInsert(Dsymbol *s);
    virtual uinteger_t size(const Loc &loc);
    virtual bool isforwardRef();
    virtual AggregateDeclaration *isThis();     // is a 'this' required to access the member
    virtual bool isExport() const;              // is Dsymbol exported?
    virtual bool isImportedSymbol() const;      // is Dsymbol imported?
    virtual bool isDeprecated() const;                // is Dsymbol deprecated?
    virtual bool isOverloadable() const;
    virtual LabelDsymbol *isLabel();            // is this a LabelDsymbol?
    AggregateDeclaration *isMember();           // is toParent() an AggregateDeclaration?
    AggregateDeclaration *isMember2();          // is toParent2() an AggregateDeclaration?
    AggregateDeclaration *isMemberDecl();       // is toParentDecl() an AggregateDeclaration?
    AggregateDeclaration *isMemberLocal();      // is toParentLocal() an AggregateDeclaration?
    ClassDeclaration *isClassMember();          // isMember() is a ClassDeclaration?
    virtual Type *getType();                    // is this a type?
    virtual bool needThis();                    // need a 'this' pointer?
    virtual Visibility visible();
    virtual Dsymbol *syntaxCopy(Dsymbol *s);    // copy only syntax trees
    virtual bool oneMember(Dsymbol **ps, Identifier *ident);
    virtual void setFieldOffset(AggregateDeclaration *ad, FieldState& fieldState, bool isunion);
    virtual bool hasPointers();
    virtual bool hasStaticCtorOrDtor();
    virtual void addObjcSymbols(ClassDeclarations *, ClassDeclarations *) { }
    virtual void checkCtorConstInit() { }

    virtual void addComment(const utf8_t *comment);
    const utf8_t *comment();                      // current value of comment

    UnitTestDeclaration *ddocUnittest();
    void ddocUnittest(UnitTestDeclaration *);

    bool inNonRoot();

    // Eliminate need for dynamic_cast
    virtual Package *isPackage() { return NULL; }
    virtual Module *isModule() { return NULL; }
    virtual EnumMember *isEnumMember() { return NULL; }
    virtual TemplateDeclaration *isTemplateDeclaration() { return NULL; }
    virtual TemplateInstance *isTemplateInstance() { return NULL; }
    virtual TemplateMixin *isTemplateMixin() { return NULL; }
    virtual ForwardingAttribDeclaration *isForwardingAttribDeclaration() { return NULL; }
    virtual Nspace *isNspace() { return NULL; }
    virtual Declaration *isDeclaration() { return NULL; }
    virtual StorageClassDeclaration *isStorageClassDeclaration(){ return NULL; }
    virtual ExpressionDsymbol *isExpressionDsymbol() { return NULL; }
    virtual AliasAssign *isAliasAssign() { return NULL; }
    virtual ThisDeclaration *isThisDeclaration() { return NULL; }
    virtual BitFieldDeclaration *isBitFieldDeclaration() { return NULL; }
    virtual TypeInfoDeclaration *isTypeInfoDeclaration() { return NULL; }
    virtual TupleDeclaration *isTupleDeclaration() { return NULL; }
    virtual AliasDeclaration *isAliasDeclaration() { return NULL; }
    virtual AggregateDeclaration *isAggregateDeclaration() { return NULL; }
    virtual FuncDeclaration *isFuncDeclaration() { return NULL; }
    virtual FuncAliasDeclaration *isFuncAliasDeclaration() { return NULL; }
    virtual OverDeclaration *isOverDeclaration() { return NULL; }
    virtual FuncLiteralDeclaration *isFuncLiteralDeclaration() { return NULL; }
    virtual CtorDeclaration *isCtorDeclaration() { return NULL; }
    virtual PostBlitDeclaration *isPostBlitDeclaration() { return NULL; }
    virtual DtorDeclaration *isDtorDeclaration() { return NULL; }
    virtual StaticCtorDeclaration *isStaticCtorDeclaration() { return NULL; }
    virtual StaticDtorDeclaration *isStaticDtorDeclaration() { return NULL; }
    virtual SharedStaticCtorDeclaration *isSharedStaticCtorDeclaration() { return NULL; }
    virtual SharedStaticDtorDeclaration *isSharedStaticDtorDeclaration() { return NULL; }
    virtual InvariantDeclaration *isInvariantDeclaration() { return NULL; }
    virtual UnitTestDeclaration *isUnitTestDeclaration() { return NULL; }
    virtual NewDeclaration *isNewDeclaration() { return NULL; }
    virtual VarDeclaration *isVarDeclaration() { return NULL; }
    virtual VersionSymbol *isVersionSymbol() { return NULL; }
    virtual DebugSymbol *isDebugSymbol() { return NULL; }
    virtual ClassDeclaration *isClassDeclaration() { return NULL; }
    virtual StructDeclaration *isStructDeclaration() { return NULL; }
    virtual UnionDeclaration *isUnionDeclaration() { return NULL; }
    virtual InterfaceDeclaration *isInterfaceDeclaration() { return NULL; }
    virtual ScopeDsymbol *isScopeDsymbol() { return NULL; }
    virtual ForwardingScopeDsymbol *isForwardingScopeDsymbol() { return NULL; }
    virtual WithScopeSymbol *isWithScopeSymbol() { return NULL; }
    virtual ArrayScopeSymbol *isArrayScopeSymbol() { return NULL; }
    virtual Import *isImport() { return NULL; }
    virtual EnumDeclaration *isEnumDeclaration() { return NULL; }
    virtual SymbolDeclaration *isSymbolDeclaration() { return NULL; }
    virtual AttribDeclaration *isAttribDeclaration() { return NULL; }
    virtual AnonDeclaration *isAnonDeclaration() { return NULL; }
    virtual CPPNamespaceDeclaration *isCPPNamespaceDeclaration() { return NULL; }
    virtual VisibilityDeclaration *isVisibilityDeclaration() { return NULL; }
    virtual OverloadSet *isOverloadSet() { return NULL; }
    virtual CompileDeclaration *isCompileDeclaration() { return NULL; }
    virtual StaticAssert *isStaticAssert() { return NULL; }
    virtual StaticIfDeclaration *isStaticIfDeclaration() { return NULL; }
    void accept(Visitor *v) override { v->visit(this); }
};

// Dsymbol that generates a scope

class ScopeDsymbol : public Dsymbol
{
public:
    Dsymbols *members;          // all Dsymbol's in this scope
    DsymbolTable *symtab;       // members[] sorted into table
    unsigned endlinnum;         // the linnumber of the statement after the scope (0 if unknown)

private:
    Dsymbols *importedScopes;   // imported Dsymbol's
    Visibility::Kind *visibilities;   // array of `Visibility.Kind`, one for each import

    BitArray accessiblePackages, privateAccessiblePackages;

public:
    ScopeDsymbol *syntaxCopy(Dsymbol *s) override;
    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = SearchLocalsOnly) override;
    virtual void importScope(Dsymbol *s, Visibility visibility);
    virtual bool isPackageAccessible(Package *p, Visibility visibility, int flags = 0);
    bool isforwardRef() override final;
    static void multiplyDefined(const Loc &loc, Dsymbol *s1, Dsymbol *s2);
    const char *kind() const override;
    FuncDeclaration *findGetMembers();
    virtual Dsymbol *symtabInsert(Dsymbol *s);
    virtual Dsymbol *symtabLookup(Dsymbol *s, Identifier *id);
    bool hasStaticCtorOrDtor() override;

    ScopeDsymbol *isScopeDsymbol() override final { return this; }
    void accept(Visitor *v) override { v->visit(this); }
};

// With statement scope

class WithScopeSymbol final : public ScopeDsymbol
{
public:
    WithStatement *withstate;

    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = SearchLocalsOnly) override;

    WithScopeSymbol *isWithScopeSymbol() override { return this; }
    void accept(Visitor *v) override { v->visit(this); }
};

// Array Index/Slice scope

class ArrayScopeSymbol final : public ScopeDsymbol
{
private:
    RootObject *arrayContent;
public:
    Scope *sc;

    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = IgnoreNone) override;

    ArrayScopeSymbol *isArrayScopeSymbol() override { return this; }
    void accept(Visitor *v) override { v->visit(this); }
};

// Overload Sets

class OverloadSet final : public Dsymbol
{
public:
    Dsymbols a;         // array of Dsymbols

    void push(Dsymbol *s);
    OverloadSet *isOverloadSet() override { return this; }
    const char *kind() const override;
    void accept(Visitor *v) override { v->visit(this); }
};

// Forwarding ScopeDsymbol

class ForwardingScopeDsymbol final : public ScopeDsymbol
{
public:
    Dsymbol *symtabInsert(Dsymbol *s) override;
    Dsymbol *symtabLookup(Dsymbol *s, Identifier *id) override;
    void importScope(Dsymbol *s, Visibility visibility) override;
    const char *kind() const override;

    ForwardingScopeDsymbol *isForwardingScopeDsymbol() override { return this; }
};

class ExpressionDsymbol final : public Dsymbol
{
public:
    Expression *exp;

    ExpressionDsymbol *isExpressionDsymbol() override { return this; }
};

// Table of Dsymbol's

class DsymbolTable final : public RootObject
{
public:
    AA *tab;

    // Look up Identifier. Return Dsymbol if found, NULL if not.
    Dsymbol *lookup(Identifier const * const ident);

    // Look for Dsymbol in table. If there, return it. If not, insert s and return that.
    void update(Dsymbol *s);

    // Insert Dsymbol in table. Return NULL if already there.
    Dsymbol *insert(Dsymbol *s);
    Dsymbol *insert(Identifier const * const ident, Dsymbol *s);     // when ident and s are not the same

    // Number of symbols in symbol table
    size_t length() const;
};
