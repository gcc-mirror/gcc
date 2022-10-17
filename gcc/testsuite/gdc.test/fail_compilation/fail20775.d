/*
TEST_OUTPUT:
---
fail_compilation/fail20775.d(19): Error: cannot pass types that need destruction as variadic arguments
fail_compilation/fail20775.d(20): Error: cannot pass types that need destruction as variadic arguments
---
*/
extern void variadic(...);

struct S20775
{
    int field;
    ~this() { }
}

void test()
{
    auto v = S20775(0);
    variadic(v,
             S20775(1));
}
