/*
TEST_OUTPUT:
---
fail_compilation/fail314.d(11): Error: declaration T is already defined
---
*/

struct foo
{
    static if (is(int T == int)) {}
    static if (is(int T == int)) {}
}
