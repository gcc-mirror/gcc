
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
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

namespace dmd
{
    Expressions *getAttributes(UserAttributeDeclaration *a);
}

/**************************************************************/

class AttribDeclaration : public Dsymbol
{
public:
    Dsymbols *decl;     // array of Dsymbol's
    const char *kind() const override;
    bool hasPointers() override final;
    void accept(Visitor *v) override { v->visit(this); }
};

class StorageClassDeclaration : public AttribDeclaration
{
public:
    StorageClass stc;

    StorageClassDeclaration *syntaxCopy(Dsymbol *s) override;

    void accept(Visitor *v) override { v->visit(this); }
};

class DeprecatedDeclaration final : public StorageClassDeclaration
{
public:
    Expression *msg;
    const char *msgstr;

    DeprecatedDeclaration *syntaxCopy(Dsymbol *s) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class LinkDeclaration final : public AttribDeclaration
{
public:
    LINK linkage;

    static LinkDeclaration *create(Loc loc, LINK p, Dsymbols *decl);
    LinkDeclaration *syntaxCopy(Dsymbol *s) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class CPPMangleDeclaration final : public AttribDeclaration
{
public:
    CPPMANGLE cppmangle;

    CPPMangleDeclaration *syntaxCopy(Dsymbol *s) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class CPPNamespaceDeclaration final : public AttribDeclaration
{
public:
    Expression *exp;

    CPPNamespaceDeclaration *syntaxCopy(Dsymbol *s) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class VisibilityDeclaration final : public AttribDeclaration
{
public:
    Visibility visibility;
    DArray<Identifier*> pkg_identifiers;

    VisibilityDeclaration *syntaxCopy(Dsymbol *s) override;
    const char *kind() const override;
    const char *toPrettyChars(bool unused) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class AlignDeclaration final : public AttribDeclaration
{
public:
    Expressions *alignExps;
    structalign_t salign;

    AlignDeclaration *syntaxCopy(Dsymbol *s) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class AnonDeclaration final : public AttribDeclaration
{
public:
    d_bool isunion;
    int sem;                    // 1 if successful semantic()
    unsigned anonoffset;        // offset of anonymous struct
    unsigned anonstructsize;    // size of anonymous struct
    unsigned anonalignsize;     // size of anonymous struct for alignment purposes

    AnonDeclaration *syntaxCopy(Dsymbol *s) override;
    const char *kind() const override;
    void accept(Visitor *v) override { v->visit(this); }
};

class PragmaDeclaration final : public AttribDeclaration
{
public:
    Expressions *args;          // array of Expression's

    PragmaDeclaration *syntaxCopy(Dsymbol *s) override;
    const char *kind() const override;
    void accept(Visitor *v) override { v->visit(this); }
};

class ConditionalDeclaration : public AttribDeclaration
{
public:
    Condition *condition;
    Dsymbols *elsedecl; // array of Dsymbol's for else block

    ConditionalDeclaration *syntaxCopy(Dsymbol *s) override;
    void accept(Visitor *v) override { v->visit(this); }
};

class StaticIfDeclaration final : public ConditionalDeclaration
{
public:
    ScopeDsymbol *scopesym;
    d_bool addisdone;
    d_bool onStack;

    StaticIfDeclaration *syntaxCopy(Dsymbol *s) override;
    const char *kind() const override;
    void accept(Visitor *v) override { v->visit(this); }
};

class StaticForeachDeclaration final : public AttribDeclaration
{
public:
    StaticForeach *sfe;
    ScopeDsymbol *scopesym;
    d_bool onStack;
    d_bool cached;
    Dsymbols *cache;

    StaticForeachDeclaration *syntaxCopy(Dsymbol *s) override;
    const char *kind() const override;
    void accept(Visitor *v) override { v->visit(this); }
};

class ForwardingAttribDeclaration final : public AttribDeclaration
{
public:
    ForwardingScopeDsymbol *sym;

    void accept(Visitor *v) override { v->visit(this); }
};

// Mixin declarations

class MixinDeclaration final : public AttribDeclaration
{
public:
    Expressions *exps;

    ScopeDsymbol *scopesym;
    d_bool compiled;

    MixinDeclaration *syntaxCopy(Dsymbol *s) override;
    const char *kind() const override;
    void accept(Visitor *v) override { v->visit(this); }
};

/**
 * User defined attributes look like:
 *      @(args, ...)
 */
class UserAttributeDeclaration final : public AttribDeclaration
{
public:
    Expressions *atts;

    UserAttributeDeclaration *syntaxCopy(Dsymbol *s) override;
    const char *kind() const override;
    void accept(Visitor *v) override { v->visit(this); }
};
