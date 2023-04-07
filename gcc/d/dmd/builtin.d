/**
 * Implement CTFE for intrinsic (builtin) functions.
 *
 * Currently includes functions from `std.math`, `core.math` and `core.bitop`.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/builtin.d, _builtin.d)
 * Documentation:  https://dlang.org/phobos/dmd_builtin.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/builtin.d
 */

module dmd.builtin;

import dmd.arraytypes;
import dmd.expression;
import dmd.func;
import dmd.location;

/**********************************
 * Determine if function is a builtin one that we can
 * evaluate at compile time.
 */
public extern (C++) BUILTIN isBuiltin(FuncDeclaration fd);

/**************************************
 * Evaluate builtin function.
 * Return result; NULL if cannot evaluate it.
 */
public extern (C++) Expression eval_builtin(const ref Loc loc, FuncDeclaration fd, Expressions* arguments);
