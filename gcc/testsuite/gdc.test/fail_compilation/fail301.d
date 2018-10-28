/*
TEST_OUTPUT:
---
fail_compilation/fail301.d(11): Error: need 'this' for 'guard' of type 'int'
fail_compilation/fail301.d(22): Error: template instance fail301.bug3305!0 error instantiating
---
*/

struct bug3305(alias X = 0)
{
    auto guard = bug3305b!(0).guard;
}

struct bug3305b(alias X = 0)
{
    bug3305!(X) goo;
    auto guard = 0;
}

void test()
{
    bug3305!(0) a;
}
