// PERMUTE_ARGS:
static import imports.bug8922;

void test()
{
    static assert(!__traits(compiles, __traits(parent, imports)));
    static assert(!__traits(compiles, __traits(parent, bug8922)));
    enum x = __traits(parent, imports.bug8922).stringof;
    static assert(x == "package imports");
}
