/**
 * Declarations for back-end functions that the front-end invokes.
 *
 * This 'glues' either the DMC or GCC back-end to the front-end.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/gluelayer.d, _gluelayer.d)
 * Documentation:  https://dlang.org/phobos/dmd_gluelayer.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/gluelayer.d
 */

module dmd.gluelayer;

import dmd.dmodule;
import dmd.dscope;
import dmd.dsymbol;
import dmd.mtype;
import dmd.statement;
import dmd.root.file;

version (NoBackend)
{
    struct Symbol;
    struct code;
    struct block;
    struct Blockx;
    struct elem;
    struct TYPE;
    alias type = TYPE;

    extern(C++) abstract class ObjcGlue
    {
        static void initialize() {}
    }
}
else version (IN_GCC)
{
    extern (C++) union tree_node;

    alias Symbol = tree_node;
    alias code = tree_node;
    alias type = tree_node;

    // stubs
    extern(C++) abstract class ObjcGlue
    {
        static void initialize() {}
    }
}
else
{
    public import dmd.backend.cc : block, Blockx, Symbol;
    public import dmd.backend.type : type;
    public import dmd.backend.el : elem;
    public import dmd.backend.code_x86 : code;
    public import dmd.objc_glue : ObjcGlue;
}
