
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/attrib.h
 */

#pragma once

#include "dsymbol.h"

class Expression;
class Statement;
class LabelDsymbol;
class Initializer;
class Module;
class Condition;
class StaticForeach;

/**************************************************************/

class AttribDeclaration : public Dsymbol
{
public:
    Dsymbols *decl;     // array of Dsymbol's

    AttribDeclaration(Dsymbols *decl);
    virtual Dsymbols *include(Scope *sc, ScopeDsymbol *sds);
    int apply(Dsymbol_apply_ft_t fp, void *param);
    static Scope *createNewScope(Scope *sc,
        StorageClass newstc, LINK linkage, CPPMANGLE cppmangle, Prot protection,
        int explicitProtection, AlignDeclaration *aligndecl, PINLINE inlining);
    virtual Scope *newScope(Scope *sc);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void setScope(Scope *sc);
    void importAll(Scope *sc);
    void semantic(Scope *sc);
    void semantic2(Scope *sc);
    void semantic3(Scope *sc);
    void addComment(const utf8_t *comment);
    const char *kind() const;
    bool oneMember(Dsymbol **ps, Identifier *ident);
    void setFieldOffset(AggregateDeclaration *ad, unsigned *poffset, bool isunion);
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

    StorageClassDeclaration(StorageClass stc, Dsymbols *decl);
    Dsymbol *syntaxCopy(Dsymbol *s);
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

    DeprecatedDeclaration(Expression *msg, Dsymbols *decl);
    Dsymbol *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    void setScope(Scope *sc);
    void semantic2(Scope *sc);
    const char *getMessage();
    void accept(Visitor *v) { v->visit(this); }
};

class LinkDeclaration : public AttribDeclaration
{
public:
    LINK linkage;

    LinkDeclaration(LINK p, Dsymbols *decl);
    static LinkDeclaration *create(LINK p, Dsymbols *decl);
    Dsymbol *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    const char *toChars();
    void accept(Visitor *v) { v->visit(this); }
};

class CPPMangleDeclaration : public AttribDeclaration
{
public:
    CPPMANGLE cppmangle;

    CPPMangleDeclaration(CPPMANGLE p, Dsymbols *decl);
    Dsymbol *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    const char *toChars();
    void accept(Visitor *v) { v->visit(this); }
};

class ProtDeclaration : public AttribDeclaration
{
public:
    Prot protection;
    Identifiers* pkg_identifiers;

    ProtDeclaration(Loc loc, Prot p, Dsymbols *decl);
    ProtDeclaration(Loc loc, Identifiers* pkg_identifiers, Dsymbols *decl);

    Dsymbol *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    const char *kind() const;
    const char *toPrettyChars(bool unused);
    void accept(Visitor *v) { v->visit(this); }
};

class AlignDeclaration : public AttribDeclaration
{
public:
    Expression *ealign;
    structalign_t salign;

    AlignDeclaration(Loc loc, Expression *ealign, Dsymbols *decl);
    Dsymbol *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    void semantic2(Scope *sc);
    structalign_t getAlignment(Scope *sc);
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

    AnonDeclaration(Loc loc, bool isunion, Dsymbols *decl);
    Dsymbol *syntaxCopy(Dsymbol *s);
    void setScope(Scope *sc);
    void semantic(Scope *sc);
    void setFieldOffset(AggregateDeclaration *ad, unsigned *poffset, bool isunion);
    const char *kind() const;
    AnonDeclaration *isAnonDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class PragmaDeclaration : public AttribDeclaration
{
public:
    Expressions *args;          // array of Expression's

    PragmaDeclaration(Loc loc, Identifier *ident, Expressions *args, Dsymbols *decl);
    Dsymbol *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    void semantic(Scope *sc);
    const char *kind() const;
    void accept(Visitor *v) { v->visit(this); }
};

class ConditionalDeclaration : public AttribDeclaration
{
public:
    Condition *condition;
    Dsymbols *elsedecl; // array of Dsymbol's for else block

    ConditionalDeclaration(Condition *condition, Dsymbols *decl, Dsymbols *elsedecl);
    Dsymbol *syntaxCopy(Dsymbol *s);
    bool oneMember(Dsymbol **ps, Identifier *ident);
    Dsymbols *include(Scope *sc, ScopeDsymbol *sds);
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

    StaticIfDeclaration(Condition *condition, Dsymbols *decl, Dsymbols *elsedecl);
    Dsymbol *syntaxCopy(Dsymbol *s);
    Dsymbols *include(Scope *sc, ScopeDsymbol *sds);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void setScope(Scope *sc);
    void importAll(Scope *sc);
    void semantic(Scope *sc);
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

    StaticForeachDeclaration(StaticForeach *sfe, Dsymbols *decl);
    Dsymbol *syntaxCopy(Dsymbol *s);
    bool oneMember(Dsymbol **ps, Identifier *ident);
    Dsymbols *include(Scope *sc, ScopeDsymbol *sds);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void addComment(const utf8_t *comment);
    void setScope(Scope *sc);
    void importAll(Scope *sc);
    void semantic(Scope *sc);
    const char *kind() const;
    void accept(Visitor *v) { v->visit(this); }
};

class ForwardingAttribDeclaration : public AttribDeclaration
{
public:
    ForwardingScopeDsymbol *sym;

    ForwardingAttribDeclaration(Dsymbols *decl);
    Scope *newScope(Scope *sc);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    ForwardingAttribDeclaration *isForwardingAttribDeclaration() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

// Mixin declarations

class CompileDeclaration : public AttribDeclaration
{
public:
    Expression *exp;

    ScopeDsymbol *scopesym;
    bool compiled;

    CompileDeclaration(Loc loc, Expression *exp);
    Dsymbol *syntaxCopy(Dsymbol *s);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void setScope(Scope *sc);
    void compileIt(Scope *sc);
    void semantic(Scope *sc);
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

    UserAttributeDeclaration(Expressions *atts, Dsymbols *decl);
    Dsymbol *syntaxCopy(Dsymbol *s);
    Scope *newScope(Scope *sc);
    void setScope(Scope *sc);
    void semantic(Scope *sc);
    void semantic2(Scope *sc);
    static Expressions *concat(Expressions *udas1, Expressions *udas2);
    Expressions *getAttributes();
    const char *kind() const;
    void accept(Visitor *v) { v->visit(this); }
};
