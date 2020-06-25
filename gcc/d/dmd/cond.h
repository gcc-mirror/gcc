
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/cond.h
 */

#pragma once

#include "ast_node.h"
#include "globals.h"
#include "visitor.h"

class Expression;
class Identifier;
struct OutBuffer;
class Module;
struct Scope;
class ScopeDsymbol;
class DebugCondition;
class ForeachStatement;
class ForeachRangeStatement;

int findCondition(Identifiers *ids, Identifier *ident);

class Condition : public ASTNode
{
public:
    Loc loc;
    // 0: not computed yet
    // 1: include
    // 2: do not include
    int inc;

    Condition(Loc loc);

    virtual Condition *syntaxCopy() = 0;
    virtual int include(Scope *sc) = 0;
    virtual DebugCondition *isDebugCondition() { return NULL; }
    virtual VersionCondition *isVersionCondition() { return NULL; }
    void accept(Visitor *v) { v->visit(this); }
};

class StaticForeach
{
public:
    Loc loc;

    ForeachStatement *aggrfe;
    ForeachRangeStatement *rangefe;

    bool needExpansion;

    StaticForeach(Loc loc, ForeachStatement *aggrfe, ForeachRangeStatement *rangefe);
    StaticForeach *syntaxCopy();
};

void staticForeachPrepare(StaticForeach *sfe, Scope *sc);
bool staticForeachReady(StaticForeach *sfe);

class DVCondition : public Condition
{
public:
    unsigned level;
    Identifier *ident;
    Module *mod;

    DVCondition(Module *mod, unsigned level, Identifier *ident);

    Condition *syntaxCopy();
    void accept(Visitor *v) { v->visit(this); }
};

class DebugCondition : public DVCondition
{
public:
    static void addGlobalIdent(const char *ident);

    DebugCondition(Module *mod, unsigned level, Identifier *ident);

    int include(Scope *sc);
    DebugCondition *isDebugCondition() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class VersionCondition : public DVCondition
{
public:
    static void addGlobalIdent(const char *ident);
    static void addPredefinedGlobalIdent(const char *ident);

    VersionCondition(Module *mod, unsigned level, Identifier *ident);

    int include(Scope *sc);
    VersionCondition *isVersionCondition() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class StaticIfCondition : public Condition
{
public:
    Expression *exp;

    StaticIfCondition(Loc loc, Expression *exp);
    Condition *syntaxCopy();
    int include(Scope *sc);
    void accept(Visitor *v) { v->visit(this); }
};
