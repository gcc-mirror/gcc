// https://issues.dlang.org/show_bug.cgi?id=18493
// REQUIRED_ARGS: -betterC

struct S
{
    this(this)
    {
    }

    ~this()
    {
    }
}

struct C
{
    S s1;
    S s2;
}
