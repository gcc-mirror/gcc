// PERMUTE_ARGS:
// EXTRA_FILES: imports/bug8922.d
import renamed = imports.bug8922;

void test()
{
    enum x = __traits(parent, renamed).stringof;
    static assert(x == "package imports");
    static assert(!__traits(compiles, __traits(parent, imports)));
    static assert(!__traits(compiles, __traits(parent, bug8922)));
    static assert(!__traits(compiles, __traits(parent, imports.bug8922)));
}
