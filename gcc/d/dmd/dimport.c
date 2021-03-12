
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/import.c
 */

#include "root/dsystem.h"
#include "root/root.h"

#include "mars.h"
#include "dsymbol.h"
#include "import.h"
#include "identifier.h"
#include "module.h"
#include "scope.h"
#include "mtype.h"
#include "declaration.h"
#include "id.h"
#include "attrib.h"
#include "hdrgen.h"

/********************************* Import ****************************/

Import::Import(Loc loc, Identifiers *packages, Identifier *id, Identifier *aliasId,
        int isstatic)
    : Dsymbol(NULL)
{
    assert(id);
    this->loc = loc;
    this->packages = packages;
    this->id = id;
    this->aliasId = aliasId;
    this->isstatic = isstatic;
    this->protection = Prot(Prot::private_); // default to private
    this->pkg = NULL;
    this->mod = NULL;

    // Set symbol name (bracketed)
    if (aliasId)
    {
        // import [cstdio] = std.stdio;
        this->ident = aliasId;
    }
    else if (packages && packages->length)
    {
        // import [std].stdio;
        this->ident = (*packages)[0];
    }
    else
    {
        // import [foo];
        this->ident = id;
    }
}

void Import::addAlias(Identifier *name, Identifier *alias)
{
    if (isstatic)
        error("cannot have an import bind list");

    if (!aliasId)
        this->ident = NULL;     // make it an anonymous import

    names.push(name);
    aliases.push(alias);
}

const char *Import::kind() const
{
    return isstatic ? "static import" : "import";
}

Prot Import::prot()
{
    return protection;
}

Dsymbol *Import::syntaxCopy(Dsymbol *s)
{
    assert(!s);

    Import *si = new Import(loc, packages, id, aliasId, isstatic);

    for (size_t i = 0; i < names.length; i++)
    {
        si->addAlias(names[i], aliases[i]);
    }

    return si;
}

void Import::load(Scope *sc)
{
    //printf("Import::load('%s') %p\n", toPrettyChars(), this);

    // See if existing module
    DsymbolTable *dst = Package::resolve(packages, NULL, &pkg);
    Dsymbol *s = dst->lookup(id);
    if (s)
    {
        if (s->isModule())
            mod = (Module *)s;
        else
        {
            if (s->isAliasDeclaration())
            {
                ::error(loc, "%s %s conflicts with %s", s->kind(), s->toPrettyChars(), id->toChars());
            }
            else if (Package *p = s->isPackage())
            {
                if (p->isPkgMod == PKGunknown)
                {
                    mod = Module::load(loc, packages, id);
                    if (!mod)
                        p->isPkgMod = PKGpackage;
                    else
                    {
                        // mod is a package.d, or a normal module which conflicts with the package name.
                        assert(mod->isPackageFile == (p->isPkgMod == PKGmodule));
                        if (mod->isPackageFile)
                            mod->tag = p->tag; // reuse the same package tag
                    }
                }
                else
                {
                    mod = p->isPackageMod();
                }
                if (!mod)
                {
                    ::error(loc, "can only import from a module, not from package %s.%s",
                        p->toPrettyChars(), id->toChars());
                }
            }
            else if (pkg)
            {
                ::error(loc, "can only import from a module, not from package %s.%s",
                    pkg->toPrettyChars(), id->toChars());
            }
            else
            {
                ::error(loc, "can only import from a module, not from package %s",
                    id->toChars());
            }
        }
    }

    if (!mod)
    {
        // Load module
        mod = Module::load(loc, packages, id);
        if (mod)
        {
            dst->insert(id, mod);           // id may be different from mod->ident,
                                            // if so then insert alias
        }
    }
    if (mod && !mod->importedFrom)
        mod->importedFrom = sc ? sc->_module->importedFrom : Module::rootModule;
    if (!pkg)
        pkg = mod;

    //printf("-Import::load('%s'), pkg = %p\n", toChars(), pkg);
}

void Import::importAll(Scope *sc)
{
    if (!mod)
    {
        load(sc);
        if (mod)                // if successfully loaded module
        {
            mod->importAll(NULL);

            if (mod->md && mod->md->isdeprecated)
            {
                Expression *msg = mod->md->msg;
                if (StringExp *se = msg ? msg->toStringExp() : NULL)
                    mod->deprecation(loc, "is deprecated - %s", se->string);
                else
                    mod->deprecation(loc, "is deprecated");
            }

            if (sc->explicitProtection)
                protection = sc->protection;
            if (!isstatic && !aliasId && !names.length)
            {
                sc->scopesym->importScope(mod, protection);
            }
        }
    }
}

Dsymbol *Import::toAlias()
{
    if (aliasId)
        return mod;
    return this;
}

/*****************************
 * Add import to sd's symbol table.
 */

void Import::addMember(Scope *sc, ScopeDsymbol *sd)
{
    //printf("Import::addMember(this=%s, sd=%s, sc=%p)\n", toChars(), sd->toChars(), sc);
    if (names.length == 0)
        return Dsymbol::addMember(sc, sd);

    if (aliasId)
        Dsymbol::addMember(sc, sd);

    /* Instead of adding the import to sd's symbol table,
     * add each of the alias=name pairs
     */
    for (size_t i = 0; i < names.length; i++)
    {
        Identifier *name = names[i];
        Identifier *alias = aliases[i];

        if (!alias)
            alias = name;

        TypeIdentifier *tname = new TypeIdentifier(loc, name);
        AliasDeclaration *ad = new AliasDeclaration(loc, alias, tname);
        ad->_import = this;
        ad->addMember(sc, sd);

        aliasdecls.push(ad);
    }
}

void Import::setScope(Scope *sc)
{
    Dsymbol::setScope(sc);
    if (aliasdecls.length)
    {
        if (!mod)
            importAll(sc);

        sc = sc->push(mod);
        sc->protection = protection;
        for (size_t i = 0; i < aliasdecls.length; i++)
        {
            AliasDeclaration *ad = aliasdecls[i];
            ad->setScope(sc);
        }
        sc = sc->pop();
    }
}

Dsymbol *Import::search(const Loc &loc, Identifier *ident, int flags)
{
    //printf("%s.Import::search(ident = '%s', flags = x%x)\n", toChars(), ident->toChars(), flags);

    if (!pkg)
    {
        load(NULL);
        mod->importAll(NULL);
        dsymbolSemantic(mod, NULL);
    }

    // Forward it to the package/module
    return pkg->search(loc, ident, flags);
}

bool Import::overloadInsert(Dsymbol *s)
{
    /* Allow multiple imports with the same package base, but disallow
     * alias collisions (Bugzilla 5412).
     */
    assert(ident && ident == s->ident);
    Import *imp;
    if (!aliasId && (imp = s->isImport()) != NULL && !imp->aliasId)
        return true;
    else
        return false;
}
