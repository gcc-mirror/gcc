// https://issues.dlang.org/show_bug.cgi?id=22084
/*
TEST_OUTPUT:
---
fail_compilation/fail22084.d(22): Error: cannot pass types that need destruction as variadic arguments
---
*/
import core.stdc.stdarg;

extern(C++) void testVariadic(int a, ...)
{
}

struct Destructor
{
    ~this() { }
}

void test()
{
    auto a0 = Destructor();
    testVariadic(1, a0);
}
