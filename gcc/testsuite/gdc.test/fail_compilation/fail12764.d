// https://issues.dlang.org/show_bug.cgi?id=12764

/*
TEST_OUTPUT:
---
fail_compilation/fail12764.d(20): Error: field `s` must be initialized in constructor
---
*/

struct S
{
    @disable this();

    this(string) { }
    int f;
}

class C
{
    this(int)
    {
        s.f = 1;  // circumvents default ctor!
    }

    S s;
}
