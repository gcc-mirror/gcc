
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/import.h
 */

#pragma once

#include "dsymbol.h"

class Identifier;
struct Scope;
class Module;
class Package;
class AliasDeclaration;

class Import : public Dsymbol
{
public:
    /* static import aliasId = pkg1.pkg2.id : alias1 = name1, alias2 = name2;
     */

    Identifiers *packages;      // array of Identifier's representing packages
    Identifier *id;             // module Identifier
    Identifier *aliasId;
    int isstatic;               // !=0 if static import
    Prot protection;

    // Pairs of alias=name to bind into current namespace
    Identifiers names;
    Identifiers aliases;

    Module *mod;
    Package *pkg;               // leftmost package/module

    AliasDeclarations aliasdecls; // corresponding AliasDeclarations for alias=name pairs

    Import(Loc loc, Identifiers *packages, Identifier *id, Identifier *aliasId,
        int isstatic);
    void addAlias(Identifier *name, Identifier *alias);
    const char *kind() const;
    Prot prot();
    Dsymbol *syntaxCopy(Dsymbol *s);    // copy only syntax trees
    void load(Scope *sc);
    void importAll(Scope *sc);
    void addPackageAccess(ScopeDsymbol *scopesym);
    Dsymbol *toAlias();
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void setScope(Scope* sc);
    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = SearchLocalsOnly);
    bool overloadInsert(Dsymbol *s);

    Import *isImport() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};
