/**
 * Enforce visibility contrains such as `public` and `private`.
 *
 * Specification: $(LINK2 https://dlang.org/spec/attribute.html#visibility_attributes, Visibility Attributes)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/access.d, _access.d)
 * Documentation:  https://dlang.org/phobos/dmd_access.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/access.d
 */

module dmd.access;

import dmd.aggregate;
import dmd.astenums;
import dmd.dclass;
import dmd.dmodule;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.errors;
import dmd.expression;
import dmd.location;
import dmd.tokens;

private enum LOG = false;


/*******************************
 * Do access check for member of this class, this class being the
 * type of the 'this' pointer used to access smember.
 * Returns true if the member is not accessible.
 */
bool checkAccess(AggregateDeclaration ad, Loc loc, Scope* sc, Dsymbol smember)
{
    static if (LOG)
    {
        printf("AggregateDeclaration::checkAccess() for %s.%s in function %s() in scope %s\n", ad.toChars(), smember.toChars(), f ? f.toChars() : null, cdscope ? cdscope.toChars() : null);
    }

    const p = smember.toParent();
    if (p && p.isTemplateInstance())
    {
        return false; // for backward compatibility
    }

    if (!symbolIsVisible(sc, smember))
    {
        error(loc, "%s `%s` %s `%s` is not accessible", ad.kind(), ad.toPrettyChars(), smember.kind(), smember.toChars());
        //printf("smember = %s %s, vis = %d, semanticRun = %d\n",
        //        smember.kind(), smember.toPrettyChars(), smember.visible() smember.semanticRun);
        return true;
    }
    return false;
}

/****************************************
 * Determine if scope sc has package level access to s.
 */
private bool hasPackageAccess(Scope* sc, Dsymbol s)
{
    return hasPackageAccess(sc._module, s);
}

private bool hasPackageAccess(Module mod, Dsymbol s)
{
    static if (LOG)
    {
        printf("hasPackageAccess(s = '%s', mod = '%s', s.visibility.pkg = '%s')\n", s.toChars(), mod.toChars(), s.visible().pkg ? s.visible().pkg.toChars() : "NULL");
    }
    Package pkg = null;
    if (s.visible().pkg)
        pkg = s.visible().pkg;
    else
    {
        // no explicit package for visibility, inferring most qualified one
        for (; s; s = s.parent)
        {
            if (auto m = s.isModule())
            {
                DsymbolTable dst = Package.resolve(m.md ? m.md.packages : null, null, null);
                assert(dst);
                Dsymbol s2 = dst.lookup(m.ident);
                assert(s2);
                Package p = s2.isPackage();
                if (p && p.isPackageMod())
                {
                    pkg = p;
                    break;
                }
            }
            else if ((pkg = s.isPackage()) !is null)
                break;
        }
    }
    static if (LOG)
    {
        if (pkg)
            printf("\tsymbol access binds to package '%s'\n", pkg.toChars());
    }
    if (pkg)
    {
        if (pkg == mod.parent)
        {
            static if (LOG)
            {
                printf("\tsc is in permitted package for s\n");
            }
            return true;
        }
        if (pkg.isPackageMod() == mod)
        {
            static if (LOG)
            {
                printf("\ts is in same package.d module as sc\n");
            }
            return true;
        }
        Dsymbol ancestor = mod.parent;
        for (; ancestor; ancestor = ancestor.parent)
        {
            if (ancestor == pkg)
            {
                static if (LOG)
                {
                    printf("\tsc is in permitted ancestor package for s\n");
                }
                return true;
            }
        }
    }
    static if (LOG)
    {
        printf("\tno package access\n");
    }
    return false;
}

/****************************************
 * Determine if scope sc has protected level access to cd.
 */
private bool hasProtectedAccess(Scope *sc, Dsymbol s)
{
    if (auto cd = s.isClassMember()) // also includes interfaces
    {
        for (auto scx = sc; scx; scx = scx.enclosing)
        {
            if (!scx.scopesym)
                continue;
            auto cd2 = scx.scopesym.isClassDeclaration();
            if (cd2 && cd.isBaseOf(cd2, null))
                return true;
        }
    }
    return sc._module == s.getAccessModule();
}

/****************************************
 * Check access to d for expression e.d
 * Returns true if the declaration is not accessible.
 */
bool checkAccess(Loc loc, Scope* sc, Expression e, Dsymbol d)
{
    if (sc.flags & SCOPE.noaccesscheck)
        return false;
    static if (LOG)
    {
        if (e)
        {
            printf("checkAccess(%s . %s)\n", e.toChars(), d.toChars());
            printf("\te.type = %s\n", e.type.toChars());
        }
        else
        {
            printf("checkAccess(%s)\n", d.toPrettyChars());
        }
    }
    if (d.isUnitTestDeclaration())
    {
        // Unittests are always accessible.
        return false;
    }

    if (!e)
        return false;

    if (auto tc = e.type.isTypeClass())
    {
        // Do access check
        ClassDeclaration cd = tc.sym;
        if (e.op == EXP.super_)
        {
            if (ClassDeclaration cd2 = sc.func.toParent().isClassDeclaration())
                cd = cd2;
        }
        return checkAccess(cd, loc, sc, d);
    }
    else if (auto ts = e.type.isTypeStruct())
    {
        // Do access check
        StructDeclaration cd = ts.sym;
        return checkAccess(cd, loc, sc, d);
    }
    return false;
}

/****************************************
 * Check access to package/module `p` from scope `sc`.
 *
 * Params:
 *   sc = scope from which to access to a fully qualified package name
 *   p = the package/module to check access for
 * Returns: true if the package is not accessible.
 *
 * Because a global symbol table tree is used for imported packages/modules,
 * access to them needs to be checked based on the imports in the scope chain
 * (see https://issues.dlang.org/show_bug.cgi?id=313).
 *
 */
bool checkAccess(Scope* sc, Package p)
{
    if (sc._module == p)
        return false;
    for (; sc; sc = sc.enclosing)
    {
        if (sc.scopesym && sc.scopesym.isPackageAccessible(p, Visibility(Visibility.Kind.private_)))
            return false;
    }

    return true;
}

/**
 * Check whether symbols `s` is visible in `mod`.
 *
 * Params:
 *  mod = lookup origin
 *  s = symbol to check for visibility
 * Returns: true if s is visible in mod
 */
bool symbolIsVisible(Module mod, Dsymbol s)
{
    // should sort overloads by ascending visibility instead of iterating here
    s = mostVisibleOverload(s);
    final switch (s.visible().kind)
    {
    case Visibility.Kind.undefined: return true;
    case Visibility.Kind.none: return false; // no access
    case Visibility.Kind.private_: return s.getAccessModule() == mod;
    case Visibility.Kind.package_: return s.getAccessModule() == mod || hasPackageAccess(mod, s);
    case Visibility.Kind.protected_: return s.getAccessModule() == mod;
    case Visibility.Kind.public_, Visibility.Kind.export_: return true;
    }
}

/**
 * Same as above, but determines the lookup module from symbols `origin`.
 */
bool symbolIsVisible(Dsymbol origin, Dsymbol s)
{
    return symbolIsVisible(origin.getAccessModule(), s);
}

/**
 * Same as above but also checks for protected symbols visible from scope `sc`.
 * Used for qualified name lookup.
 *
 * Params:
 *  sc = lookup scope
 *  s = symbol to check for visibility
 * Returns: true if s is visible by origin
 */
bool symbolIsVisible(Scope *sc, Dsymbol s)
{
    s = mostVisibleOverload(s);
    return checkSymbolAccess(sc, s);
}

/**
 * Check if a symbol is visible from a given scope without taking
 * into account the most visible overload.
 *
 * Params:
 *  sc = lookup scope
 *  s = symbol to check for visibility
 * Returns: true if s is visible by origin
 */
bool checkSymbolAccess(Scope *sc, Dsymbol s)
{
    final switch (s.visible().kind)
    {
    case Visibility.Kind.undefined: return true;
    case Visibility.Kind.none: return false; // no access
    case Visibility.Kind.private_: return sc._module == s.getAccessModule();
    case Visibility.Kind.package_: return sc._module == s.getAccessModule() || hasPackageAccess(sc._module, s);
    case Visibility.Kind.protected_: return hasProtectedAccess(sc, s);
    case Visibility.Kind.public_, Visibility.Kind.export_: return true;
    }
}

/**
 * Use the most visible overload to check visibility. Later perform an access
 * check on the resolved overload.  This function is similar to overloadApply,
 * but doesn't recurse nor resolve aliases because visibility is an
 * attribute of the alias not the aliasee.
 */
public Dsymbol mostVisibleOverload(Dsymbol s, Module mod = null)
{
    if (!s.isOverloadable())
        return s;

    Dsymbol next, fstart = s, mostVisible = s;
    for (; s; s = next)
    {
        // void func() {}
        // private void func(int) {}
        if (auto fd = s.isFuncDeclaration())
            next = fd.overnext;
        // template temp(T) {}
        // private template temp(T:int) {}
        else if (auto td = s.isTemplateDeclaration())
            next = td.overnext;
        // alias common = mod1.func1;
        // alias common = mod2.func2;
        else if (auto fa = s.isFuncAliasDeclaration())
            next = fa.overnext;
        // alias common = mod1.templ1;
        // alias common = mod2.templ2;
        else if (auto od = s.isOverDeclaration())
            next = od.overnext;
        // alias name = sym;
        // private void name(int) {}
        else if (auto ad = s.isAliasDeclaration())
        {
            assert(ad.isOverloadable || ad.type && ad.type.ty == Terror,
                "Non overloadable Aliasee in overload list");
            // Yet unresolved aliases store overloads in overnext.
            if (ad.semanticRun < PASS.semanticdone)
                next = ad.overnext;
            else
            {
                /* This is a bit messy due to the complicated implementation of
                 * alias.  Aliases aren't overloadable themselves, but if their
                 * Aliasee is overloadable they can be converted to an overloadable
                 * alias.
                 *
                 * This is done by replacing the Aliasee w/ FuncAliasDeclaration
                 * (for functions) or OverDeclaration (for templates) which are
                 * simply overloadable aliases w/ weird names.
                 *
                 * Usually aliases should not be resolved for visibility checking
                 * b/c public aliases to private symbols are public. But for the
                 * overloadable alias situation, the Alias (_ad_) has been moved
                 * into its own Aliasee, leaving a shell that we peel away here.
                 */
                auto aliasee = ad.toAlias();
                if (aliasee.isFuncAliasDeclaration || aliasee.isOverDeclaration)
                    next = aliasee;
                else
                {
                    /* A simple alias can be at the end of a function or template overload chain.
                     * It can't have further overloads b/c it would have been
                     * converted to an overloadable alias.
                     */
                    assert(ad.overnext is null, "Unresolved overload of alias");
                    break;
                }
            }
            // handled by dmd.func.overloadApply for unknown reason
            assert(next !is ad); // should not alias itself
            assert(next !is fstart); // should not alias the overload list itself
        }
        else
            break;

        /**
        * Return the "effective" visibility attribute of a symbol when accessed in a module.
        * The effective visibility attribute is the same as the regular visibility attribute,
        * except package() is "private" if the module is outside the package;
        * otherwise, "public".
        */
        static Visibility visibilitySeenFromModule(Dsymbol d, Module mod = null)
        {
            Visibility vis = d.visible();
            if (mod && vis.kind == Visibility.Kind.package_)
            {
                return hasPackageAccess(mod, d) ? Visibility(Visibility.Kind.public_) : Visibility(Visibility.Kind.private_);
            }
            return vis;
        }

        if (next &&
            visibilitySeenFromModule(mostVisible, mod) < visibilitySeenFromModule(next, mod))
            mostVisible = next;
    }
    return mostVisible;
}
