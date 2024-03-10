// PERMUTE_ARGS: -w -wi -debug
@safe pure nothrow void strictVoidReturn(T)(T x) {}
@safe pure nothrow void nonstrictVoidReturn(T)(ref T x) {}

void test3882()
{
    int x = 3;
    strictVoidReturn(x);
    nonstrictVoidReturn(x);
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12619

extern (C) @system nothrow pure void* memcpy(void* s1, const void* s2, size_t n);
// -> weakly pure

void test12619() pure
{
    ubyte[10] a, b;
    debug memcpy(a.ptr, b.ptr, 5);  // memcpy call should have side effect
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12760

struct S12760(T)
{
    T i;
    this(T j) inout {}
}

struct K12760
{
    S12760!int nullable;

    this(int)
    {
        nullable = 0;   // weak purity
    }
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=12909

int f12909(immutable(int[])[int] aa) pure nothrow
{
    //aa[0] = [];   // fix for https://issues.dlang.org/show_bug.cgi?id=13701
    return 0;
}

void test12909()
{
    immutable(int[])[int] aa;
    f12909(aa);

    // from 12910
    const(int[])[int] makeAA() { return null; }  // to make r-value
    makeAA().rehash();
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=13899

const struct Foo13899
{
    int opApply(immutable int delegate(const ref int) pure nothrow dg) pure nothrow
    {
        return 1;
    }
}

void test13899()
{
    foreach (x; Foo13899())
    {
    }
}
