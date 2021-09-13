
// Compiler implementation of the D programming language
// Copyright: Copyright (C) 2014-2021 by The D Language Foundation, All Rights Reserved
// Authors: Walter Bright, http://www.digitalmars.com
// License: http://boost.org/LICENSE_1_0.txt
// Source: https://github.com/D-Programming-Language/dmd/blob/master/src/nspace.c


#include "root/dsystem.h"

#include "mars.h"
#include "dsymbol.h"
#include "nspace.h"
#include "identifier.h"
#include "scope.h"

/* This implements namespaces.
 */

Nspace::Nspace(Loc loc, Identifier *ident, Dsymbols *members, bool mangleOnly)
    : ScopeDsymbol(ident)
{
    //printf("Nspace::Nspace(ident = %s)\n", ident->toChars());
    this->loc = loc;
    this->members = members;
    // Determines whether the symbol for this namespace should be included in
    // the symbol table.
    this->mangleOnly = mangleOnly;
}

Dsymbol *Nspace::syntaxCopy(Dsymbol *)
{
    Nspace *ns = new Nspace(loc, ident, NULL, mangleOnly);
    return ScopeDsymbol::syntaxCopy(ns);
}

void Nspace::addMember(Scope *sc, ScopeDsymbol *sds)
{
    if (mangleOnly)
        parent = sds;
    else
        ScopeDsymbol::addMember(sc, sds);
    if (members)
    {
        if (!symtab)
            symtab = new DsymbolTable();
        // The namespace becomes 'imported' into the enclosing scope
        for (Scope *sce = sc; 1; sce = sce->enclosing)
        {
            ScopeDsymbol *sds2 = sce->scopesym;
            if (sds2)
            {
                sds2->importScope(this, Prot(Prot::public_));
                break;
            }
        }
        assert(sc);
        sc = sc->push(this);
        sc->linkage = LINKcpp; // namespaces default to C++ linkage
        sc->parent = this;
        for (size_t i = 0; i < members->length; i++)
        {
            Dsymbol *s = (*members)[i];
            //printf("add %s to scope %s\n", s->toChars(), toChars());
            s->addMember(sc, this);
        }
        sc->pop();
    }
}

void Nspace::setScope(Scope *sc)
{
    ScopeDsymbol::setScope(sc);
    if (members)
    {
        assert(sc);
        sc = sc->push(this);
        sc->linkage = LINKcpp; // namespaces default to C++ linkage
        sc->parent = this;
        for (size_t i = 0; i < members->length; i++)
        {
            Dsymbol *s = (*members)[i];
            s->setScope(sc);
        }
        sc->pop();
    }
}

const char *Nspace::kind() const
{
    return "namespace";
}

bool Nspace::oneMember(Dsymbol **ps, Identifier *ident)
{
    return Dsymbol::oneMember(ps, ident);
}

Dsymbol *Nspace::search(const Loc &loc, Identifier *ident, int flags)
{
    //printf("%s::Nspace::search('%s')\n", toChars(), ident->toChars());
    if (_scope && !symtab)
        dsymbolSemantic(this, _scope);

    if (!members || !symtab) // opaque or semantic() is not yet called
    {
        error("is forward referenced when looking for `%s`", ident->toChars());
        return NULL;
    }

    return ScopeDsymbol::search(loc, ident, flags);
}

int Nspace::apply(Dsymbol_apply_ft_t fp, void *param)
{
    if (members)
    {
        for (size_t i = 0; i < members->length; i++)
        {
            Dsymbol *s = (*members)[i];
            if (s)
            {
                if (s->apply(fp, param))
                    return 1;
            }
        }
    }
    return 0;
}

bool Nspace::hasPointers()
{
    //printf("Nspace::hasPointers() %s\n", toChars());

    if (members)
    {
        for (size_t i = 0; i < members->length; i++)
        {
            Dsymbol *s = (*members)[i];
            //printf(" s = %s %s\n", s->kind(), s->toChars());
            if (s->hasPointers())
            {
                return true;
            }
        }
    }
    return false;
}

void Nspace::setFieldOffset(AggregateDeclaration *ad, unsigned *poffset, bool isunion)
{
    //printf("Nspace::setFieldOffset() %s\n", toChars());
    if (_scope)                  // if fwd reference
        dsymbolSemantic(this, NULL);         // try to resolve it
    if (members)
    {
        for (size_t i = 0; i < members->length; i++)
        {
            Dsymbol *s = (*members)[i];
            //printf("\t%s\n", s->toChars());
            s->setFieldOffset(ad, poffset, isunion);
        }
    }
}
