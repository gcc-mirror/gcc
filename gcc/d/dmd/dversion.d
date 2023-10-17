/**
 * Defines a `Dsymbol` for `version = identifier` and `debug = identifier` statements.
 *
 * Specification: $(LINK2 https://dlang.org/spec/version.html#version-specification, Version Specification),
 *                $(LINK2 https://dlang.org/spec/version.html#debug_specification, Debug Specification).
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/dversion.d, _dversion.d)
 * Documentation:  https://dlang.org/phobos/dmd_dversion.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/dversion.d
 */

module dmd.dversion;

import dmd.arraytypes;
import dmd.cond;
import dmd.dmodule;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.errors;
import dmd.globals;
import dmd.identifier;
import dmd.location;
import dmd.common.outbuffer;
import dmd.visitor;

/***********************************************************
 * DebugSymbol's happen for statements like:
 *      debug = identifier;
 *      debug = integer;
 */
extern (C++) final class DebugSymbol : Dsymbol
{
    uint level;

    extern (D) this(const ref Loc loc, Identifier ident) @safe
    {
        super(loc, ident);
    }

    extern (D) this(const ref Loc loc, uint level) @safe
    {
        super(loc, null);
        this.level = level;
    }

    override DebugSymbol syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto ds = new DebugSymbol(loc, ident);
        ds.comment = comment;
        ds.level = level;
        return ds;
    }

    override const(char)* toChars() const nothrow
    {
        if (ident)
            return ident.toChars();
        else
        {
            OutBuffer buf;
            buf.print(level);
            return buf.extractChars();
        }
    }

    override void addMember(Scope* sc, ScopeDsymbol sds)
    {
        //printf("DebugSymbol::addMember('%s') %s\n", sds.toChars(), toChars());
        Module m = sds.isModule();
        // Do not add the member to the symbol table,
        // just make sure subsequent debug declarations work.
        if (ident)
        {
            if (!m)
            {
                .error(loc, "%s `%s` declaration must be at module level", kind, toPrettyChars);
                errors = true;
            }
            else
            {
                if (findCondition(m.debugidsNot, ident))
                {
                    .error(loc, "%s `%s` defined after use", kind, toPrettyChars);
                    errors = true;
                }
                if (!m.debugids)
                    m.debugids = new Identifiers();
                m.debugids.push(ident);
            }
        }
        else
        {
            if (!m)
            {
                .error(loc, "%s `%s` level declaration must be at module level", kind, toPrettyChars);
                errors = true;
            }
            else
                m.debuglevel = level;
        }
    }

    override const(char)* kind() const nothrow
    {
        return "debug";
    }

    override inout(DebugSymbol) isDebugSymbol() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * VersionSymbol's happen for statements like:
 *      version = identifier;
 *      version = integer;
 */
extern (C++) final class VersionSymbol : Dsymbol
{
    uint level;

    extern (D) this(const ref Loc loc, Identifier ident) @safe
    {
        super(loc, ident);
    }

    extern (D) this(const ref Loc loc, uint level) @safe
    {
        super(loc, null);
        this.level = level;
    }

    override VersionSymbol syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto ds = ident ? new VersionSymbol(loc, ident)
                        : new VersionSymbol(loc, level);
        ds.comment = comment;
        return ds;
    }

    override const(char)* toChars() const nothrow
    {
        if (ident)
            return ident.toChars();
        else
        {
            OutBuffer buf;
            buf.print(level);
            return buf.extractChars();
        }
    }

    override void addMember(Scope* sc, ScopeDsymbol sds)
    {
        //printf("VersionSymbol::addMember('%s') %s\n", sds.toChars(), toChars());
        Module m = sds.isModule();
        // Do not add the member to the symbol table,
        // just make sure subsequent debug declarations work.
        if (ident)
        {
            VersionCondition.checkReserved(loc, ident.toString());
            if (!m)
            {
                .error(loc, "%s `%s` declaration must be at module level", kind, toPrettyChars);
                errors = true;
            }
            else
            {
                if (findCondition(m.versionidsNot, ident))
                {
                    .error(loc, "%s `%s` defined after use", kind, toPrettyChars);
                    errors = true;
                }
                if (!m.versionids)
                    m.versionids = new Identifiers();
                m.versionids.push(ident);
            }
        }
        else
        {
            if (!m)
            {
                .error(loc, "%s `%s` level declaration must be at module level", kind, toPrettyChars);
                errors = true;
            }
            else
                m.versionlevel = level;
        }
    }

    override const(char)* kind() const nothrow
    {
        return "version";
    }

    override inout(VersionSymbol) isVersionSymbol() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}
