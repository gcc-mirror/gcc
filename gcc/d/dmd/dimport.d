/**
 * A `Dsymbol` representing a renamed import.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/dimport.d, _dimport.d)
 * Documentation:  https://dlang.org/phobos/dmd_dimport.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/dimport.d
 */

module dmd.dimport;

import dmd.arraytypes;
import dmd.astenums;
import dmd.declaration;
import dmd.dmodule;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.errors;
import dmd.expression;
import dmd.globals;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.visitor;

import core.stdc.stdio;
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

    extern (D) this(const ref Loc loc, Identifier[] packages, Identifier id, Identifier aliasId, int isstatic)
    {
        Identifier selectIdent()
        {
            // select Dsymbol identifier (bracketed)
            if (aliasId)
            {
                // import [aliasId] = std.stdio;
                return aliasId;
            }
            else if (packages.length > 0)
            {
                // import [std].stdio;
                return packages[0];
            }
            else
            {
                // import [id];
                return id;
            }
        }

        super(loc, selectIdent());

        assert(id);
        version (none)
        {
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

    extern (D) void addAlias(Identifier name, Identifier _alias)
    {
        if (isstatic)
            .error(loc, "%s `%s` cannot have an import bind list", kind, toPrettyChars);
        if (!aliasId)
            this.ident = null; // make it an anonymous import
        names.push(name);
        aliases.push(_alias);
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
        for (size_t i = 0; i < names.length; i++)
        {
            si.addAlias(names[i], aliases[i]);
        }
        return si;
    }

    /*******************************
     * Load this module.
     * Returns:
     *  true for errors, false for success
     */
    extern (D) bool load(Scope* sc)
    {
        //printf("Import::load('%s') %p\n", toPrettyChars(), this);
        // See if existing module
        const errors = global.errors;
        DsymbolTable dst = Package.resolve(packages, null, &pkg);
        version (none)
        {
            if (pkg && pkg.isModule())
            {
                .error(loc, "can only import from a module, not from a member of module `%s`. Did you mean `import %s : %s`?", pkg.toChars(), pkg.toPrettyChars(), id.toChars());
                mod = pkg.isModule(); // Error recovery - treat as import of that module
                return true;
            }
        }
        Dsymbol s = dst.lookup(id);
        if (s)
        {
            if (s.isModule())
                mod = cast(Module)s;
            else
            {
                if (s.isAliasDeclaration())
                {
                    .error(loc, "%s `%s` conflicts with `%s`", s.kind(), s.toPrettyChars(), id.toChars());
                }
                else if (Package p = s.isPackage())
                {
                    if (p.isPkgMod == PKG.unknown)
                    {
                        uint preverrors = global.errors;
                        mod = Module.load(loc, packages, id);
                        if (!mod)
                            p.isPkgMod = PKG.package_;
                        else
                        {
                            // mod is a package.d, or a normal module which conflicts with the package name.
                            if (mod.isPackageFile)
                                mod.tag = p.tag; // reuse the same package tag
                            else
                            {
                                // show error if Module.load does not
                                if (preverrors == global.errors)
                                    .error(loc, "%s `%s` from file %s conflicts with %s `%s`", mod.kind(), mod.toPrettyChars(), mod.srcfile.toChars, p.kind(), p.toPrettyChars());
                                return true;
                            }
                        }
                    }
                    else
                    {
                        mod = p.isPackageMod();
                    }
                    if (!mod)
                    {
                        .error(loc, "can only import from a module, not from package `%s.%s`", p.toPrettyChars(), id.toChars());
                    }
                }
                else if (pkg)
                {
                    .error(loc, "can only import from a module, not from package `%s.%s`", pkg.toPrettyChars(), id.toChars());
                }
                else
                {
                    .error(loc, "can only import from a module, not from package `%s`", id.toChars());
                }
            }
        }
        if (!mod)
        {
            // Load module
            mod = Module.load(loc, packages, id);
            if (mod)
            {
                // id may be different from mod.ident, if so then insert alias
                dst.insert(id, mod);
            }
        }
        if (mod && !mod.importedFrom)
            mod.importedFrom = sc ? sc._module.importedFrom : Module.rootModule;
        if (!pkg)
        {
            if (mod && mod.isPackageFile)
            {
                // one level depth package.d file (import pkg; ./pkg/package.d)
                // it's necessary to use the wrapping Package already created
                pkg = mod.pkg;
            }
            else
                pkg = mod;
        }
        //printf("-Import::load('%s'), pkg = %p\n", toChars(), pkg);
        return global.errors != errors;
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
        Import imp;
        if (!aliasId && (imp = s.isImport()) !is null && !imp.aliasId)
            return true;
        else
            return false;
    }

    override inout(Import) isImport() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}
