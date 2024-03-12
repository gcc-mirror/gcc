// EXTRA_FILES: imports/library.c

// https://issues.dlang.org/show_bug.cgi?id=23913

import imports.library;

alias x = __traits(getMember, imports.library, "SomeEnum");
