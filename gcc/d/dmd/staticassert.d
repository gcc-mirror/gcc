/**
 * Defines the `Dsymbol` representing a `static assert()`.
 *
 * Specification: $(LINK2 https://dlang.org/spec/version.html#static-assert, Static Assert)
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/staticassert.d, _staticassert.d)
 * Documentation:  https://dlang.org/phobos/dmd_staticassert.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/staticassert.d
 */

module dmd.staticassert;

import dmd.arraytypes;
import dmd.dsymbol;
import dmd.expression;
import dmd.location;
import dmd.id;
import dmd.identifier;
import dmd.visitor;

/***********************************************************
 */
extern (C++) final class StaticAssert : Dsymbol
{
    Expression exp;
    Expressions* msgs;

    extern (D) this(Loc loc, Expression exp, Expression msg)
    {
        super(DSYM.staticAssert, loc, Id.empty);
        this.exp = exp;
        this.msgs = new Expressions(1);
        (*this.msgs)[0] = msg;
    }

    extern (D) this(Loc loc, Expression exp, Expressions* msgs)
    {
        super(DSYM.staticAssert, loc, Id.empty);
        this.exp = exp;
        this.msgs = msgs;
    }

    override StaticAssert syntaxCopy(Dsymbol s)
    {
        assert(!s);
        return new StaticAssert(loc, exp.syntaxCopy(), msgs ? Expression.arraySyntaxCopy(msgs) : null);
    }

    override const(char)* kind() const
    {
        return "static assert";
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}
