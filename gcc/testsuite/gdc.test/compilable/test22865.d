// https://issues.dlang.org/show_bug.cgi?id=22865

// Test that safety errors inside speculative scopes don't affect attribute inference

void main() @safe
{
    foo();
}

__gshared int g;

auto foo()
{
    alias x0 = typeof(g++);
    alias x1 = typeof(cast(int*) 0);

    auto x2 = __traits(compiles, g++);
    enum x3 = __traits(compiles, (cast(int*) 0));

    debug
    {
        g++;
        const x4 = cast(int*) 0;
        asm { }
    }
}

// Test that safety violations still occur if the function is inside the __traits(compiles)

static assert(!__traits(compiles, {
    void f() @safe
    {
        g++;
    }
}));
