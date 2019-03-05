// PERMUTE_ARGS:
void test()
{
    static import imports.bug8922;
    static assert(!__traits(compiles, __traits(parent, imports)));
    static assert(!__traits(compiles, __traits(parent, bug8922)));
    enum x = __traits(parent, imports.bug8922).stringof;
    static assert(x == "package imports");
}

