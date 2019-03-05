// https://issues.dlang.org/show_bug.cgi?id=17421

import core.stdc.stdarg;

void novar() {}
extern(C) void cstyle(int, ...) {}
extern(C++) void cppstyle(int, ...) {}
void dstyle(...) {}
void typesafe(int[]...) {}

static assert(__traits(getFunctionVariadicStyle, novar) == "none");
static assert(__traits(getFunctionVariadicStyle, cstyle) == "stdarg");
static assert(__traits(getFunctionVariadicStyle, cppstyle) == "stdarg");
static assert(__traits(getFunctionVariadicStyle, dstyle) == "argptr");
static assert(__traits(getFunctionVariadicStyle, typesafe) == "typesafe");

static assert(__traits(getFunctionVariadicStyle, (int[] a...) {}) == "typesafe");
static assert(__traits(getFunctionVariadicStyle, typeof(cstyle)) == "stdarg");

