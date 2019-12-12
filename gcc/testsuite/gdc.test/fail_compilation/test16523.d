// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/test16523.d(13): Deprecation: case variables have to be const or immutable
---
*/

void test(int a, int b)
{
    switch (a)
    {
    case b: return;
    default: assert(0);
    }
}
