
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
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

void Import::semantic(Scope *sc)
{
    //printf("Import::semantic('%s') %s\n", toPrettyChars(), id->toChars());
    if (semanticRun > PASSinit)
        return;

    if (_scope)
    {
        sc = _scope;
        _scope = NULL;
    }
    if (!sc)
        return;

    semanticRun = PASSsemantic;

    // Load if not already done so
    if (!mod)
    {
        load(sc);
        if (mod)
            mod->importAll(NULL);
    }

    if (mod)
    {
        // Modules need a list of each imported module
        //printf("%s imports %s\n", sc->_module->toChars(), mod->toChars());
        sc->_module->aimports.push(mod);

        if (sc->explicitProtection)
            protection = sc->protection;

        if (!aliasId && !names.length) // neither a selective nor a renamed import
        {
            ScopeDsymbol *scopesym = NULL;
            if (sc->explicitProtection)
                protection = sc->protection.kind;
            for (Scope *scd = sc; scd; scd = scd->enclosing)
            {
                if (!scd->scopesym)
                    continue;
                scopesym = scd->scopesym;
                break;
            }

            if (!isstatic)
            {
                scopesym->importScope(mod, protection);
            }

            // Mark the imported packages as accessible from the current
            // scope. This access check is necessary when using FQN b/c
            // we're using a single global package tree. See Bugzilla 313.
            if (packages)
            {
                // import a.b.c.d;
                Package *p = pkg; // a
                scopesym->addAccessiblePackage(p, protection);
                for (size_t i = 1; i < packages->length; i++) // [b, c]
                {
                    Identifier *id = (*packages)[i];
                    p = (Package *) p->symtab->lookup(id);
                    scopesym->addAccessiblePackage(p, protection);
                }
            }
            scopesym->addAccessiblePackage(mod, protection); // d
        }

        mod->semantic(NULL);

        if (mod->needmoduleinfo)
        {
            //printf("module4 %s because of %s\n", sc->_module->toChars(), mod->toChars());
            sc->_module->needmoduleinfo = 1;
        }

        sc = sc->push(mod);
        sc->protection = protection;
        for (size_t i = 0; i < aliasdecls.length; i++)
        {
            AliasDeclaration *ad = aliasdecls[i];
            //printf("\tImport %s alias %s = %s, scope = %p\n", toPrettyChars(), aliases[i]->toChars(), names[i]->toChars(), ad->_scope);
            if (mod->search(loc, names[i]))
            {
                ad->semantic(sc);
                // If the import declaration is in non-root module,
                // analysis of the aliased symbol is deferred.
                // Therefore, don't see the ad->aliassym or ad->type here.
            }
            else
            {
                Dsymbol *s = mod->search_correct(names[i]);
                if (s)
                    mod->error(loc, "import '%s' not found, did you mean %s '%s'?", names[i]->toChars(), s->kind(), s->toChars());
                else
                    mod->error(loc, "import '%s' not found", names[i]->toChars());
                ad->type = Type::terror;
            }
        }
        sc = sc->pop();
    }

    semanticRun = PASSsemanticdone;

    // object self-imports itself, so skip that (Bugzilla 7547)
    // don't list pseudo modules __entrypoint.d, __main.d (Bugzilla 11117, 11164)
    if (global.params.moduleDeps != NULL &&
        !(id == Id::object && sc->_module->ident == Id::object) &&
        sc->_module->ident != Id::entrypoint &&
        strcmp(sc->_module->ident->toChars(), "__main") != 0)
    {
        /* The grammar of the file is:
         *      ImportDeclaration
         *          ::= BasicImportDeclaration [ " : " ImportBindList ] [ " -> "
         *      ModuleAliasIdentifier ] "\n"
         *
         *      BasicImportDeclaration
         *          ::= ModuleFullyQualifiedName " (" FilePath ") : " Protection|"string"
         *              " [ " static" ] : " ModuleFullyQualifiedName " (" FilePath ")"
         *
         *      FilePath
         *          - any string with '(', ')' and '\' escaped with the '\' character
         */

        OutBuffer *ob = global.params.moduleDeps;
        Module* imod = sc->instantiatingModule();
        if (!global.params.moduleDepsFile.length)
            ob->writestring("depsImport ");
        ob->writestring(imod->toPrettyChars());
        ob->writestring(" (");
        escapePath(ob,  imod->srcfile->toChars());
        ob->writestring(") : ");

        // use protection instead of sc->protection because it couldn't be
        // resolved yet, see the comment above
        protectionToBuffer(ob, protection);
        ob->writeByte(' ');
        if (isstatic)
        {
            stcToBuffer(ob, STCstatic);
            ob->writeByte(' ');
        }
        ob->writestring(": ");

        if (packages)
        {
            for (size_t i = 0; i < packages->length; i++)
            {
                Identifier *pid = (*packages)[i];
                ob->printf("%s.", pid->toChars());
            }
        }

        ob->writestring(id->toChars());
        ob->writestring(" (");
        if (mod)
            escapePath(ob, mod->srcfile->toChars());
        else
            ob->writestring("???");
        ob->writeByte(')');

        for (size_t i = 0; i < names.length; i++)
        {
            if (i == 0)
                ob->writeByte(':');
            else
                ob->writeByte(',');

            Identifier *name = names[i];
            Identifier *alias = aliases[i];

            if (!alias)
            {
                ob->printf("%s", name->toChars());
                alias = name;
            }
            else
                ob->printf("%s=%s", alias->toChars(), name->toChars());
        }

        if (aliasId)
                ob->printf(" -> %s", aliasId->toChars());

        ob->writenl();
    }

    //printf("-Import::semantic('%s'), pkg = %p\n", toChars(), pkg);
}

void Import::semantic2(Scope *sc)
{
    //printf("Import::semantic2('%s')\n", toChars());
    if (mod)
    {
        mod->semantic2(NULL);
        if (mod->needmoduleinfo)
        {
            //printf("module5 %s because of %s\n", sc->_module->toChars(), mod->toChars());
            if (sc)
                sc->_module->needmoduleinfo = 1;
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
        mod->semantic(NULL);
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
