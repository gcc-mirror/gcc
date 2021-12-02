// PERMUTE_ARGS:
// EXTRA_FILES: imports/bug8922.d
import imports.bug8922;

void test()
{
    static assert(!__traits(compiles, __traits(parent, imports)));
    enum x = __traits(parent, imports.bug8922).stringof;
    static assert(x == "package imports");
}
