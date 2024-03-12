/**
 * A scoped C++ namespace symbol
 *
 * D supports the following syntax to declare symbol(s) as being part of a
 * C++ namespace:
 * ---
 * extern (C++, "myNamespace") { /+ Symbols +/ }       // String variant
 * extern (C++, SomeNamespace) { /+ Other symbols +/ } // Identifier variant
 * ---
 * The first form is an attribute and only affects mangling, and is implemented
 * in `dmd.attrib`.
 * The second form introduces a named scope and allows symbols to be refered
 * to with or without the namespace name, much like a named template mixin,
 * and is implemented in this module.
 * ---
 * extern (C++, Basket)
 * {
 *     struct StrawBerry;
 *     void swapFood (Strawberry* f1, Strawberry* f2);
 * }
 * void main ()
 * {
 *     Basket.StrawBerry fruit1;
 *     StrawBerry fruit2;
 *     Basket.swapFood(fruit1, fruit2);
 *     swapFood(fruit1, fruit2);
 * }
 * ---
 * Hence the `Nspace` symbol implements the usual `ScopeDsymbol` semantics.
 *
 * Note that it implies `extern(C++)` so it cannot be used as a generic
 * named scope. Additionally, `Nspace` with the same `Identifier` can be
 * defined in different module (as C++ allows a namespace to be spread accross
 * translation units), but symbols in it should be considered
 * part of the same scope. Lastly, not all possible C++ namespace names
 * are valid D identifier.
 *
 * See_Also:    https://github.com/dlang/dmd/pull/10031
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/nspace.d, _nspace.d)
 * Documentation:  https://dlang.org/phobos/dmd_nspace.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/nspace.d
 */

module dmd.nspace;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.errors;
import dmd.expression;
import dmd.globals;
import dmd.identifier;
import dmd.location;
import dmd.visitor;
import core.stdc.stdio;

private enum LOG = false;

/// Ditto
extern (C++) final class Nspace : ScopeDsymbol
{
    /**
     * Namespace identifier resolved during semantic.
     */
    Expression identExp;

    extern (D) this(const ref Loc loc, Identifier ident, Expression identExp, Dsymbols* members)
    {
        super(loc, ident);
        //printf("Nspace::Nspace(ident = %s)\n", ident.toChars());
        this.members = members;
        this.identExp = identExp;
    }

    override Nspace syntaxCopy(Dsymbol s)
    {
        auto ns = new Nspace(loc, ident, identExp, null);
        ScopeDsymbol.syntaxCopy(ns);
        return ns;
    }

    override void addMember(Scope* sc, ScopeDsymbol sds)
    {
        ScopeDsymbol.addMember(sc, sds);

        if (members)
        {
            if (!symtab)
                symtab = new DsymbolTable();
            // The namespace becomes 'imported' into the enclosing scope
            for (Scope* sce = sc; 1; sce = sce.enclosing)
            {
                ScopeDsymbol sds2 = sce.scopesym;
                if (sds2)
                {
                    sds2.importScope(this, Visibility(Visibility.Kind.public_));
                    break;
                }
            }
            assert(sc);
            sc = sc.push(this);
            sc.linkage = LINK.cpp; // namespaces default to C++ linkage
            sc.parent = this;
            members.foreachDsymbol(s => s.addMember(sc, this));
            sc.pop();
        }
    }

    override void setScope(Scope* sc)
    {
        ScopeDsymbol.setScope(sc);
        if (members)
        {
            assert(sc);
            sc = sc.push(this);
            sc.linkage = LINK.cpp; // namespaces default to C++ linkage
            sc.parent = this;
            members.foreachDsymbol(s => s.setScope(sc));
            sc.pop();
        }
    }

    override Dsymbol search(const ref Loc loc, Identifier ident, int flags = SearchLocalsOnly)
    {
        //printf("%s.Nspace.search('%s')\n", toChars(), ident.toChars());
        if (_scope && !symtab)
            dsymbolSemantic(this, _scope);

        if (!members || !symtab) // opaque or semantic() is not yet called
        {
            if (!(flags & IgnoreErrors))
                .error(loc, "%s `%s` is forward referenced when looking for `%s`", kind, toPrettyChars, ident.toChars());
            return null;
        }

        return ScopeDsymbol.search(loc, ident, flags);
    }

    override bool hasPointers()
    {
        //printf("Nspace::hasPointers() %s\n", toChars());
        return members.foreachDsymbol( (s) { return s.hasPointers(); } ) != 0;
    }

    override void setFieldOffset(AggregateDeclaration ad, ref FieldState fieldState, bool isunion)
    {
        //printf("Nspace::setFieldOffset() %s\n", toChars());
        if (_scope) // if fwd reference
            dsymbolSemantic(this, null); // try to resolve it
        members.foreachDsymbol( s => s.setFieldOffset(ad, fieldState, isunion) );
    }

    override const(char)* kind() const
    {
        return "namespace";
    }

    override inout(Nspace) isNspace() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}
