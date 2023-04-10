/**
 * Describes a back-end compiler and implements compiler-specific actions.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/compiler.d, _compiler.d)
 * Documentation:  https://dlang.org/phobos/dmd_compiler.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/compiler.d
 */

module dmd.compiler;

import dmd.arraytypes;
import dmd.dmodule;
import dmd.dscope;
import dmd.expression;
import dmd.mtype;
import dmd.root.array;

extern (C++) __gshared
{
    bool includeImports = false;
    // array of module patterns used to include/exclude imported modules
    Array!(const(char)*) includeModulePatterns;
    Modules compiledImports;
}


/**
 * A data structure that describes a back-end compiler and implements
 * compiler-specific actions.
 */
extern (C++) struct Compiler
{
    /******************************
     * Encode the given expression, which is assumed to be an rvalue literal
     * as another type for use in CTFE.
     * This corresponds roughly to the idiom *(Type *)&e.
     */
    extern (C++) static Expression paintAsType(UnionExp* pue, Expression e, Type type);

    /******************************
     * For the given module, perform any post parsing analysis.
     * Certain compiler backends (ie: GDC) have special placeholder
     * modules whose source are empty, but code gets injected
     * immediately after loading.
     */
    extern (C++) static void onParseModule(Module m);

    /**
     * A callback function that is called once an imported module is
     * parsed. If the callback returns true, then it tells the
     * frontend that the driver intends on compiling the import.
     */
    extern (C++) static bool onImport(Module m);
}
