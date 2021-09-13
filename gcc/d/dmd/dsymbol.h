
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/dsymbol.h
 */

#pragma once

#include "root/port.h"
#include "root/stringtable.h"
#include "ast_node.h"
#include "globals.h"
#include "arraytypes.h"
#include "visitor.h"

class Identifier;
struct Scope;
class DsymbolTable;
class Declaration;
class ThisDeclaration;
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
class DeleteDeclaration;
class OverloadSet;
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

void dsymbolSemantic(Dsymbol *dsym, Scope *sc);
void semantic2(Dsymbol *dsym, Scope* sc);
void semantic3(Dsymbol *dsym, Scope* sc);

struct Prot
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

    Prot();
    Prot(Kind kind);

    bool isMoreRestrictiveThan(const Prot other) const;
    bool operator==(const Prot& other) const;
};

// in hdrgen.c
void protectionToBuffer(OutBuffer *buf, Prot prot);
const char *protectionToChars(Prot::Kind kind);

/* State of symbol in winding its way through the passes of the compiler
 */
enum PASS
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
    IgnoreSymbolVisibility  = 0x80  // also find private and package protected symbols
};

typedef int (*Dsymbol_apply_ft_t)(Dsymbol *, void *);

class Dsymbol : public ASTNode
{
public:
    Identifier *ident;
    Dsymbol *parent;
    Symbol *csym;               // symbol for code generator
    Symbol *isym;               // import version of csym
    const utf8_t *comment;      // documentation comment for this Dsymbol
    Loc loc;                    // where defined
    Scope *_scope;               // !=NULL means context to use for semantic()
    const utf8_t *prettystring;
    bool errors;                // this symbol failed to pass semantic()
    PASS semanticRun;
    DeprecatedDeclaration *depdecl; // customized deprecation message
    UserAttributeDeclaration *userAttribDecl;   // user defined attributes
    UnitTestDeclaration *ddocUnittest; // !=NULL means there's a ddoc unittest associated with this symbol (only use this with ddoc)

    Dsymbol();
    Dsymbol(Identifier *);
    static Dsymbol *create(Identifier *);
    const char *toChars();
    virtual const char *toPrettyCharsHelper(); // helper to print fully qualified (template) arguments
    Loc& getLoc();
    const char *locToChars();
    bool equals(RootObject *o);
    bool isAnonymous();
    void error(Loc loc, const char *format, ...);
    void error(const char *format, ...);
    void deprecation(Loc loc, const char *format, ...);
    void deprecation(const char *format, ...);
    bool checkDeprecated(Loc loc, Scope *sc);
    Module *getModule();
    Module *getAccessModule();
    Dsymbol *pastMixin();
    Dsymbol *pastMixinAndNspace();
    Dsymbol *toParent();
    Dsymbol *toParent2();
    Dsymbol *toParent3();
    TemplateInstance *isInstantiated();
    TemplateInstance *isSpeculative();
    Ungag ungagSpeculative();

    // kludge for template.isSymbol()
    int dyncast() const { return DYNCAST_DSYMBOL; }

    static Dsymbols *arraySyntaxCopy(Dsymbols *a);

    virtual Identifier *getIdent();
    virtual const char *toPrettyChars(bool QualifyTypes = false);
    virtual const char *kind() const;
    virtual Dsymbol *toAlias();                 // resolve real symbol
    virtual Dsymbol *toAlias2();
    virtual int apply(Dsymbol_apply_ft_t fp, void *param);
    virtual void addMember(Scope *sc, ScopeDsymbol *sds);
    virtual void setScope(Scope *sc);
    virtual void importAll(Scope *sc);
    virtual Dsymbol *search(const Loc &loc, Identifier *ident, int flags = IgnoreNone);
    Dsymbol *search_correct(Identifier *id);
    Dsymbol *searchX(Loc loc, Scope *sc, RootObject *id, int flags);
    virtual bool overloadInsert(Dsymbol *s);
    virtual d_uns64 size(Loc loc);
    virtual bool isforwardRef();
    virtual AggregateDeclaration *isThis();     // is a 'this' required to access the member
    virtual bool isExport() const;              // is Dsymbol exported?
    virtual bool isImportedSymbol() const;      // is Dsymbol imported?
    virtual bool isDeprecated();                // is Dsymbol deprecated?
    virtual bool isOverloadable();
    virtual LabelDsymbol *isLabel();            // is this a LabelDsymbol?
    AggregateDeclaration *isMember();           // is this a member of an AggregateDeclaration?
    AggregateDeclaration *isMember2();          // is this a member of an AggregateDeclaration?
    ClassDeclaration *isClassMember();          // is this a member of a ClassDeclaration?
    virtual Type *getType();                    // is this a type?
    virtual bool needThis();                    // need a 'this' pointer?
    virtual Prot prot();
    virtual Dsymbol *syntaxCopy(Dsymbol *s);    // copy only syntax trees
    virtual bool oneMember(Dsymbol **ps, Identifier *ident);
    static bool oneMembers(Dsymbols *members, Dsymbol **ps, Identifier *ident);
    virtual void setFieldOffset(AggregateDeclaration *ad, unsigned *poffset, bool isunion);
    virtual bool hasPointers();
    virtual bool hasStaticCtorOrDtor();
    virtual void addLocalClass(ClassDeclarations *) { }
    virtual void checkCtorConstInit() { }

    virtual void addComment(const utf8_t *comment);

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
    virtual ThisDeclaration *isThisDeclaration() { return NULL; }
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
    virtual DeleteDeclaration *isDeleteDeclaration() { return NULL; }
    virtual SymbolDeclaration *isSymbolDeclaration() { return NULL; }
    virtual AttribDeclaration *isAttribDeclaration() { return NULL; }
    virtual AnonDeclaration *isAnonDeclaration() { return NULL; }
    virtual OverloadSet *isOverloadSet() { return NULL; }
    void accept(Visitor *v) { v->visit(this); }
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
    Prot::Kind *prots;            // array of PROTKIND, one for each import

    BitArray accessiblePackages, privateAccessiblePackages;

public:
    ScopeDsymbol();
    ScopeDsymbol(Identifier *id);
    Dsymbol *syntaxCopy(Dsymbol *s);
    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = SearchLocalsOnly);
    OverloadSet *mergeOverloadSet(Identifier *ident, OverloadSet *os, Dsymbol *s);
    virtual void importScope(Dsymbol *s, Prot protection);
    void addAccessiblePackage(Package *p, Prot protection);
    virtual bool isPackageAccessible(Package *p, Prot protection, int flags = 0);
    bool isforwardRef();
    static void multiplyDefined(Loc loc, Dsymbol *s1, Dsymbol *s2);
    const char *kind() const;
    FuncDeclaration *findGetMembers();
    virtual Dsymbol *symtabInsert(Dsymbol *s);
    virtual Dsymbol *symtabLookup(Dsymbol *s, Identifier *id);
    bool hasStaticCtorOrDtor();

    static size_t dim(Dsymbols *members);
    static Dsymbol *getNth(Dsymbols *members, size_t nth, size_t *pn = NULL);

    ScopeDsymbol *isScopeDsymbol() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

// With statement scope

class WithScopeSymbol : public ScopeDsymbol
{
public:
    WithStatement *withstate;

    WithScopeSymbol(WithStatement *withstate);
    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = SearchLocalsOnly);

    WithScopeSymbol *isWithScopeSymbol() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

// Array Index/Slice scope

class ArrayScopeSymbol : public ScopeDsymbol
{
public:
    Expression *exp;    // IndexExp or SliceExp
    TypeTuple *type;    // for tuple[length]
    TupleDeclaration *td;       // for tuples of objects
    Scope *sc;

    ArrayScopeSymbol(Scope *sc, Expression *e);
    ArrayScopeSymbol(Scope *sc, TypeTuple *t);
    ArrayScopeSymbol(Scope *sc, TupleDeclaration *td);
    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = IgnoreNone);

    ArrayScopeSymbol *isArrayScopeSymbol() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

// Overload Sets

class OverloadSet : public Dsymbol
{
public:
    Dsymbols a;         // array of Dsymbols

    OverloadSet(Identifier *ident, OverloadSet *os = NULL);
    void push(Dsymbol *s);
    OverloadSet *isOverloadSet() { return this; }
    const char *kind() const;
    void accept(Visitor *v) { v->visit(this); }
};

// Forwarding ScopeDsymbol

class ForwardingScopeDsymbol : public ScopeDsymbol
{
public:
    ScopeDsymbol *forward;

    ForwardingScopeDsymbol(ScopeDsymbol *forward);
    Dsymbol *symtabInsert(Dsymbol *s);
    Dsymbol *symtabLookup(Dsymbol *s, Identifier *id);
    void importScope(Dsymbol *s, Prot protection);
    const char *kind() const;

    ForwardingScopeDsymbol *isForwardingScopeDsymbol() { return this; }
};

// Table of Dsymbol's

class DsymbolTable : public RootObject
{
public:
    AA *tab;

    DsymbolTable();

    // Look up Identifier. Return Dsymbol if found, NULL if not.
    Dsymbol *lookup(Identifier const * const ident);

    // Insert Dsymbol in table. Return NULL if already there.
    Dsymbol *insert(Dsymbol *s);

    // Look for Dsymbol in table. If there, return it. If not, insert s and return that.
    Dsymbol *update(Dsymbol *s);
    Dsymbol *insert(Identifier const * const ident, Dsymbol *s);     // when ident and s are not the same
};
