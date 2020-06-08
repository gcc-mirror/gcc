
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/version.c
 */

#include "root/dsystem.h"
#include "root/root.h"

#include "identifier.h"
#include "dsymbol.h"
#include "cond.h"
#include "version.h"
#include "module.h"

void checkReserved(Loc loc, const char *ident);

/* ================================================== */

/* DebugSymbol's happen for statements like:
 *      debug = identifier;
 *      debug = integer;
 */

DebugSymbol::DebugSymbol(Loc loc, Identifier *ident)
    : Dsymbol(ident)
{
    this->loc = loc;
}

DebugSymbol::DebugSymbol(Loc loc, unsigned level)
    : Dsymbol()
{
    this->level = level;
    this->loc = loc;
}

const char *DebugSymbol::toChars()
{
    if (ident)
        return ident->toChars();
    else
    {
        OutBuffer buf;
        buf.printf("%d", level);
        return buf.extractChars();
    }
}

Dsymbol *DebugSymbol::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    DebugSymbol *ds = new DebugSymbol(loc, ident);
    ds->level = level;
    return ds;
}

void DebugSymbol::addMember(Scope *, ScopeDsymbol *sds)
{
    //printf("DebugSymbol::addMember('%s') %s\n", sds->toChars(), toChars());
    Module *m = sds->isModule();

    // Do not add the member to the symbol table,
    // just make sure subsequent debug declarations work.
    if (ident)
    {
        if (!m)
        {
            error("declaration must be at module level");
            errors = true;
        }
        else
        {
            if (findCondition(m->debugidsNot, ident))
            {
                error("defined after use");
                errors = true;
            }
            if (!m->debugids)
                m->debugids = new Identifiers();
            m->debugids->push(ident);
        }
    }
    else
    {
        if (!m)
        {
            error("level declaration must be at module level");
            errors = true;
        }
        else
            m->debuglevel = level;
    }
}

void DebugSymbol::semantic(Scope *)
{
    //printf("DebugSymbol::semantic() %s\n", toChars());
    if (semanticRun < PASSsemanticdone)
        semanticRun = PASSsemanticdone;
}

const char *DebugSymbol::kind() const
{
    return "debug";
}

/* ================================================== */

/* VersionSymbol's happen for statements like:
 *      version = identifier;
 *      version = integer;
 */

VersionSymbol::VersionSymbol(Loc loc, Identifier *ident)
    : Dsymbol(ident)
{
    this->loc = loc;
}

VersionSymbol::VersionSymbol(Loc loc, unsigned level)
    : Dsymbol()
{
    this->level = level;
    this->loc = loc;
}

const char *VersionSymbol::toChars()
{
    if (ident)
        return ident->toChars();
    else
    {
        OutBuffer buf;
        buf.printf("%d", level);
        return buf.extractChars();
    }
}

Dsymbol *VersionSymbol::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    VersionSymbol *ds = ident ? new VersionSymbol(loc, ident)
                              : new VersionSymbol(loc, level);
    return ds;
}

void VersionSymbol::addMember(Scope *, ScopeDsymbol *sds)
{
    //printf("VersionSymbol::addMember('%s') %s\n", sds->toChars(), toChars());
    Module *m = sds->isModule();

    // Do not add the member to the symbol table,
    // just make sure subsequent debug declarations work.
    if (ident)
    {
        checkReserved(loc, ident->toChars());
        if (!m)
        {
            error("declaration must be at module level");
            errors = true;
        }
        else
        {
            if (findCondition(m->versionidsNot, ident))
            {
                error("defined after use");
                errors = true;
            }
            if (!m->versionids)
                m->versionids = new Identifiers();
            m->versionids->push(ident);
        }
    }
    else
    {
        if (!m)
        {
            error("level declaration must be at module level");
            errors = true;
        }
        else
            m->versionlevel = level;
    }
}

void VersionSymbol::semantic(Scope *)
{
    if (semanticRun < PASSsemanticdone)
        semanticRun = PASSsemanticdone;
}

const char *VersionSymbol::kind() const
{
    return "version";
}
