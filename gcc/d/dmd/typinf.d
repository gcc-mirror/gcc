/**
 * Generate `TypeInfo` objects, which are needed for run-time introspection of classes.
 *
 * Copyright:   Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 http://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/typeinf.d, _typeinf.d)
 * Documentation:  https://dlang.org/phobos/dmd_typinf.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/typinf.d
 */

module dmd.typinf;

import dmd.dscope;
import dmd.globals;
import dmd.mtype;

/****************************************************
 * Gets the type of the `TypeInfo` object associated with `t`
 * Params:
 *      loc = the location for reporting line nunbers in errors
 *      t   = the type to get the type of the `TypeInfo` object for
 *      sc  = the scope
 * Returns:
 *      The type of the `TypeInfo` object associated with `t`
 */
extern (C++) Type getTypeInfoType(Loc loc, Type t, Scope* sc);

