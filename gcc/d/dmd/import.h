
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/import.h
 */

#pragma once

#include "dsymbol.h"

class Identifier;
struct Scope;
class Module;
class Package;

class Import final : public Dsymbol
{
public:
    /* static import aliasId = pkg1.pkg2.id : alias1 = name1, alias2 = name2;
     */

    DArray<Identifier*> packages;      // array of Identifier's representing packages
    Identifier *id;             // module Identifier
    Identifier *aliasId;
    int isstatic;               // !=0 if static import
    Visibility visibility;

    // Pairs of alias=name to bind into current namespace
    Identifiers names;
    Identifiers aliases;

    Module *mod;
    Package *pkg;               // leftmost package/module

    AliasDeclarations aliasdecls; // corresponding AliasDeclarations for alias=name pairs

    const char *kind() const override;
    Visibility visible() override;
    Import *syntaxCopy(Dsymbol *s) override; // copy only syntax trees
    Dsymbol *toAlias() override;
    bool overloadInsert(Dsymbol *s) override;

    Import *isImport() override { return this; }
    void accept(Visitor *v) override { v->visit(this); }
};
