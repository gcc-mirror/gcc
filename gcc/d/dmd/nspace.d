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
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/nspace.d, _nspace.d)
 * Documentation:  https://dlang.org/phobos/dmd_nspace.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/nspace.d
 */

module dmd.nspace;

import dmd.arraytypes;
import dmd.dsymbol;
import dmd.expression;
import dmd.identifier;
import dmd.location;
import dmd.visitor;
import core.stdc.stdio;

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

    override bool hasPointers()
    {
        //printf("Nspace::hasPointers() %s\n", toChars());
        return members.foreachDsymbol( (s) { return s.hasPointers(); } ) != 0;
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
