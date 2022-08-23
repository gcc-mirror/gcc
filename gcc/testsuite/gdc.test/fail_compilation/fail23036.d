// https://issues.dlang.org/show_bug.cgi?id=23036

/*
TEST_OUTPUT:
---
fail_compilation/fail23036.d(12): Error: `struct S` may not define both a rvalue constructor and a copy constructor
fail_compilation/fail23036.d(15):        rvalue constructor defined here
fail_compilation/fail23036.d(14):        copy constructor defined here
---
*/

struct S
{
    this(ref S) {}
    this(S, int a = 2) {}
}

void main()
{
    S a;
    S b = a;
}
