// https://issues.dlang.org/show_bug.cgi?id=18719

// REQUIRED_ARGS:
/*
TEST_OUTPUT:
---
fail_compilation/fail18719.d(30): Error: immutable field `x` initialized multiple times
       Previous initialization is here.
---
*/

struct S
{
    int x = -1;
    this(int y) immutable
    {
        x = y;
        import std.stdio;
        writeln("Ctor called with ", y);
    }
    void opAssign(int) immutable;
}

class C
{
    S x;
    this() immutable
    {
        this(42); /* Initializes x. */
        x = 13; /* Breaking immutable, or ok? */
    }
    this(int x) immutable
    {
        this.x = x;
    }
}

void main()
{
    new immutable C;
}
