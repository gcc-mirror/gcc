template forward(args...)
{
    @property fwd()() { return args[0]; }
    static assert(__traits(compiles, { auto ex = fwd; }));
    alias fwd forward;
}

void initializeClassInstance(C, Args...)(C chunk, auto ref Args args)
{
    chunk.__ctor(forward!args);
}

void main()
{
    static int si = 0;
    static class C { this(int) { ++si; } }
    void[__traits(classInstanceSize, C)] buff = void;
    auto c = cast(C) buff.ptr;
    initializeClassInstance(c, 0);
    assert(si);
}
