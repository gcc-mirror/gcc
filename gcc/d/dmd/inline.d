/**
 * Performs inlining, which is an optimization pass enabled with the `-inline` flag.
 *
 * The AST is traversed, and every function call is considered for inlining using `inlinecost.d`.
 * The function call is then inlined if this cost is below a threshold.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/inline.d, _inline.d)
 * Documentation:  https://dlang.org/phobos/dmd_inline.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/inline.d
 */

module dmd.inline;

import dmd.dscope;
import dmd.expression;

/***********************************************************
 * Perform the "inline copying" of a default argument for a function parameter.
 *
 * Todo:
 *  The hack for https://issues.dlang.org/show_bug.cgi?id=4820 case is still questionable.
 *  Perhaps would have to handle a delegate expression with 'null' context properly in front-end.
 */
public Expression inlineCopy(Expression e, Scope* sc)
{
    return e.copy();
}
