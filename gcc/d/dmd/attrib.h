
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2022 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/attrib.h
 */

#pragma once

#include "root/port.h"
#include "dsymbol.h"

class Expression;
class Condition;
class StaticForeach;

/**************************************************************/

class AttribDeclaration : public Dsymbol
{
public:
    Dsymbols *decl;     // array of Dsymbol's

    virtual Dsymbols *include(Scope *sc);
    virtual Scope *newScope(Scope *sc);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void setScope(Scope *sc);
    void importAll(Scope *sc);
    void addComment(const utf8_t *comment);
    const char *kind() const;
    bool oneMember(Dsymbol **ps, Identifier *ident);
    void setFieldOffset(AggregateDeclaration *ad, FieldState& fieldState, bool isunion);
    bool hasPointers();
    bool hasStaticCtorOrDtor();
    void checkCtorConstInit();
    void addLocalClass(ClassDeclarations *);
    AttribDeclaration *isAttribDeclaration() { return this; }

    void accept(Visitor *v) { v->visit(this); }
};

class StorageClassDeclaration : public AttribDeclaration
{
public:
    StorageClass stc;

    StorageClassDeclaration *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    bool oneMember(Dsymbol **ps, Identifier *ident);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    StorageClassDeclaration *isStorageClassDeclaration() { return this; }

    void accept(Visitor *v) { v->visit(this); }
};

class DeprecatedDeclaration : public StorageClassDeclaration
{
public:
    Expression *msg;
    const char *msgstr;

    DeprecatedDeclaration *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    void setScope(Scope *sc);
    void accept(Visitor *v) { v->visit(this); }
};

class LinkDeclaration : public AttribDeclaration
{
public:
    LINK linkage;

    static LinkDeclaration *create(const Loc &loc, LINK p, Dsymbols *decl);
    LinkDeclaration *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    const char *toChars() const;
    void accept(Visitor *v) { v->visit(this); }
};

class CPPMangleDeclaration : public AttribDeclaration
{
public:
    CPPMANGLE cppmangle;

    CPPMangleDeclaration *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    void setScope(Scope *sc);
    const char *toChars() const;
    void accept(Visitor *v) { v->visit(this); }
};

class CPPNamespaceDeclaration : public AttribDeclaration
{
public:
    Expression *exp;

    CPPNamespaceDeclaration *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    const char *toChars() const;
    void accept(Visitor *v) { v->visit(this); }
};

class VisibilityDeclaration : public AttribDeclaration
{
public:
    Visibility visibility;
    DArray<Identifier*> pkg_identifiers;

    VisibilityDeclaration *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    const char *kind() const;
    const char *toPrettyChars(bool unused);
    VisibilityDeclaration *isVisibilityDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class AlignDeclaration : public AttribDeclaration
{
public:
    Expressions *alignExps;
    structalign_t salign;

    AlignDeclaration(const Loc &loc, Expression *ealign, Dsymbols *decl);
    AlignDeclaration *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    void accept(Visitor *v) { v->visit(this); }
};

class AnonDeclaration : public AttribDeclaration
{
public:
    bool isunion;
    int sem;                    // 1 if successful semantic()
    unsigned anonoffset;        // offset of anonymous struct
    unsigned anonstructsize;    // size of anonymous struct
    unsigned anonalignsize;     // size of anonymous struct for alignment purposes

    AnonDeclaration *syntaxCopy(Dsymbol *s);
    void setScope(Scope *sc);
    void setFieldOffset(AggregateDeclaration *ad, FieldState& fieldState, bool isunion);
    const char *kind() const;
    AnonDeclaration *isAnonDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class PragmaDeclaration : public AttribDeclaration
{
public:
    Expressions *args;          // array of Expression's

    PragmaDeclaration *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    PINLINE evalPragmaInline(Scope* sc);
    const char *kind() const;
    void accept(Visitor *v) { v->visit(this); }
};

class ConditionalDeclaration : public AttribDeclaration
{
public:
    Condition *condition;
    Dsymbols *elsedecl; // array of Dsymbol's for else block

    ConditionalDeclaration *syntaxCopy(Dsymbol *s);
    bool oneMember(Dsymbol **ps, Identifier *ident);
    Dsymbols *include(Scope *sc);
    void addComment(const utf8_t *comment);
    void setScope(Scope *sc);
    void accept(Visitor *v) { v->visit(this); }
};

class StaticIfDeclaration : public ConditionalDeclaration
{
public:
    ScopeDsymbol *scopesym;
    bool addisdone;
    bool onStack;

    StaticIfDeclaration *syntaxCopy(Dsymbol *s);
    Dsymbols *include(Scope *sc);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void setScope(Scope *sc);
    void importAll(Scope *sc);
    const char *kind() const;
    void accept(Visitor *v) { v->visit(this); }
};

class StaticForeachDeclaration : public AttribDeclaration
{
public:
    StaticForeach *sfe;
    ScopeDsymbol *scopesym;
    bool onStack;
    bool cached;
    Dsymbols *cache;

    StaticForeachDeclaration *syntaxCopy(Dsymbol *s);
    bool oneMember(Dsymbol **ps, Identifier *ident);
    Dsymbols *include(Scope *sc);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void addComment(const utf8_t *comment);
    void setScope(Scope *sc);
    void importAll(Scope *sc);
    const char *kind() const;
    void accept(Visitor *v) { v->visit(this); }
};

class ForwardingAttribDeclaration : public AttribDeclaration
{
public:
    ForwardingScopeDsymbol *sym;

    Scope *newScope(Scope *sc);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    ForwardingAttribDeclaration *isForwardingAttribDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

// Mixin declarations

class CompileDeclaration : public AttribDeclaration
{
public:
    Expressions *exps;

    ScopeDsymbol *scopesym;
    bool compiled;

    CompileDeclaration *syntaxCopy(Dsymbol *s);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void setScope(Scope *sc);
    const char *kind() const;
    void accept(Visitor *v) { v->visit(this); }
};

/**
 * User defined attributes look like:
 *      @(args, ...)
 */
class UserAttributeDeclaration : public AttribDeclaration
{
public:
    Expressions *atts;

    UserAttributeDeclaration *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    void setScope(Scope *sc);
    Expressions *getAttributes();
    const char *kind() const;
    void accept(Visitor *v) { v->visit(this); }
};
