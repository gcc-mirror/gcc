
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/enum.h
 */

#pragma once

#include "root/root.h"
#include "dsymbol.h"
#include "declaration.h"
#include "tokens.h"

class Identifier;
class Type;
class Expression;
class VarDeclaration;

class EnumDeclaration : public ScopeDsymbol
{
public:
    /* The separate, and distinct, cases are:
     *  1. enum { ... }
     *  2. enum : memtype { ... }
     *  3. enum id { ... }
     *  4. enum id : memtype { ... }
     *  5. enum id : memtype;
     *  6. enum id;
     */
    Type *type;                 // the TypeEnum
    Type *memtype;              // type of the members
    Prot protection;

    Expression *maxval;
    Expression *minval;
    Expression *defaultval;     // default initializer

    bool isdeprecated;
    bool added;
    int inuse;

    EnumDeclaration(Loc loc, Identifier *id, Type *memtype);
    Dsymbol *syntaxCopy(Dsymbol *s);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void setScope(Scope *sc);
    void semantic(Scope *sc);
    bool oneMember(Dsymbol **ps, Identifier *ident);
    Type *getType();
    const char *kind() const;
    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = SearchLocalsOnly);
    bool isDeprecated();                // is Dsymbol deprecated?
    Prot prot();
    Expression *getMaxMinValue(Loc loc, Identifier *id);
    bool isSpecial() const;
    Expression *getDefaultValue(Loc loc);
    Type *getMemtype(Loc loc);

    EnumDeclaration *isEnumDeclaration() { return this; }

    Symbol *sinit;
    void accept(Visitor *v) { v->visit(this); }
};


class EnumMember : public VarDeclaration
{
public:
    /* Can take the following forms:
     *  1. id
     *  2. id = value
     *  3. type id = value
     */
    Expression *&value();

    // A cast() is injected to 'value' after semantic(),
    // but 'origValue' will preserve the original value,
    // or previous value + 1 if none was specified.
    Expression *origValue;
    Type *origType;

    EnumDeclaration *ed;

    EnumMember(Loc loc, Identifier *id, Expression *value, Type *origType);
    Dsymbol *syntaxCopy(Dsymbol *s);
    const char *kind() const;
    void semantic(Scope *sc);
    Expression *getVarExp(Loc loc, Scope *sc);

    EnumMember *isEnumMember() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};
