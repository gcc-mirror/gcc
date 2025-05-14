
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/enum.h
 */

#pragma once

#include "dsymbol.h"
#include "declaration.h"

class Identifier;
class Type;
class Expression;

namespace dmd
{
    // in enumsem.d
    Expression *getDefaultValue(EnumDeclaration *ed, Loc loc);
}

class EnumDeclaration final : public ScopeDsymbol
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
    Visibility visibility;

    Expression *maxval;
    Expression *minval;
    Expression *defaultval;     // default initializer
private:
    uint8_t bitFields;
public:
    bool isdeprecated() const;
    bool isdeprecated(bool v);
    bool added() const;
    bool added(bool v);
    bool inuse() const;
    bool inuse(bool v);

    EnumDeclaration *syntaxCopy(Dsymbol *s) override;
    Type *getType() override;
    const char *kind() const override;
    bool isDeprecated() const override;       // is Dsymbol deprecated?
    Visibility visible() override;
    bool isSpecial() const;


    Symbol *sinit;
    void accept(Visitor *v) override { v->visit(this); }
};


class EnumMember final : public VarDeclaration
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

    EnumMember *syntaxCopy(Dsymbol *s) override;
    const char *kind() const override;

    void accept(Visitor *v) override { v->visit(this); }
};
