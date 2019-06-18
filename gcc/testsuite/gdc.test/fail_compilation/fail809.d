// REQUIRED_ARGS: -preview=dip1000
/*
TEST_OUTPUT:
---
fail_compilation/fail809.d(11): Error: scope variable `dg_` may not be returned
---
*/
int delegate() test(lazy int dg)
{
    int delegate() dg_ = &dg;
    return dg_;
}
