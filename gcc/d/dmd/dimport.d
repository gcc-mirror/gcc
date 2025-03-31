/**
 * A `Dsymbol` representing a renamed import.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dimport.d, _dimport.d)
 * Documentation:  https://dlang.org/phobos/dmd_dimport.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/dimport.d
 */

module dmd.dimport;

import dmd.arraytypes;
import dmd.dmodule;
import dmd.dsymbol;
import dmd.identifier;
import dmd.location;
import dmd.visitor;

/***********************************************************
 */
extern (C++) final class Import : Dsymbol
{
    /* static import aliasId = pkg1.pkg2.id : alias1 = name1, alias2 = name2;
     */
    Identifier[] packages;  // array of Identifier's representing packages
    Identifier id;          // module Identifier
    Identifier aliasId;
    int isstatic;           // !=0 if static import
    Visibility visibility;

    // Pairs of alias=name to bind into current namespace
    Identifiers names;
    Identifiers aliases;

    Module mod;
    Package pkg;            // leftmost package/module

    // corresponding AliasDeclarations for alias=name pairs
    AliasDeclarations aliasdecls;

    extern (D) this(Loc loc, Identifier[] packages, Identifier id, Identifier aliasId, int isstatic)
    {
        Identifier selectIdent()
        {
            // select Dsymbol identifier (bracketed)
            if (aliasId)
            {
                // import [aliasId] = std.stdio;
                return aliasId;
            }
            if (packages.length > 0)
            {
                // import [std].stdio;
                return packages[0];
            }
            // import [id];
            return id;
        }

        super(DSYM.import_, loc, selectIdent());

        assert(id);
        version (none)
        {
            import core.stdc.stdio;

            printf("Import::Import(");
            foreach (id; packages)
            {
                printf("%s.", id.toChars());
            }
            printf("%s)\n", id.toChars());
        }
        this.packages = packages;
        this.id = id;
        this.aliasId = aliasId;
        this.isstatic = isstatic;
        this.visibility = Visibility.Kind.private_; // default to private
    }

    override const(char)* kind() const
    {
        return isstatic ? "static import" : "import";
    }

    override Visibility visible() pure nothrow @nogc @safe
    {
        return visibility;
    }

    // copy only syntax trees
    override Import syntaxCopy(Dsymbol s)
    {
        assert(!s);
        auto si = new Import(loc, packages, id, aliasId, isstatic);
        si.comment = comment;
        assert(!(isstatic && names.length));
        if (names.length && !si.aliasId)
            si.ident = null;
        for (size_t i = 0; i < names.length; i++)
        {
            si.names.push(names[i]);
            si.aliases.push(aliases[i]);
        }
        return si;
    }

    /*******************************
     * Mark the imported packages as accessible from the current
     * scope. This access check is necessary when using FQN b/c
     * we're using a single global package tree.
     * https://issues.dlang.org/show_bug.cgi?id=313
     */
    extern (D) void addPackageAccess(ScopeDsymbol scopesym)
    {
        //printf("Import::addPackageAccess('%s') %p\n", toPrettyChars(), this);
        if (packages.length > 0)
        {
            // import a.b.c.d;
            auto p = pkg; // a
            scopesym.addAccessiblePackage(p, visibility);
            foreach (id; packages[1 .. $]) // [b, c]
            {
                auto sym = p.symtab.lookup(id);
                // https://issues.dlang.org/show_bug.cgi?id=17991
                // An import of truly empty file/package can happen
                // https://issues.dlang.org/show_bug.cgi?id=20151
                // Package in the path conflicts with a module name
                if (sym is null)
                    break;
                // https://issues.dlang.org/show_bug.cgi?id=23327
                // Package conflicts with symbol of the same name
                p = sym.isPackage();
                if (p is null)
                    break;
                scopesym.addAccessiblePackage(p, visibility);
            }
        }
        scopesym.addAccessiblePackage(mod, visibility); // d
     }

    override Dsymbol toAlias()
    {
        if (aliasId)
            return mod;
        return this;
    }

    override bool overloadInsert(Dsymbol s)
    {
        /* Allow multiple imports with the same package base, but disallow
         * alias collisions
         * https://issues.dlang.org/show_bug.cgi?id=5412
         */
        assert(ident && ident == s.ident);
        if (aliasId)
            return false;
        const imp = s.isImport();
        return imp && !imp.aliasId;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}
