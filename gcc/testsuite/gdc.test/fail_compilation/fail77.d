/*
TEST_OUTPUT:
---
fail_compilation/fail77.d(11): Error: cannot cast expression `& i` of type `int*` to `ubyte[4]`
---
*/
void test()
{
    int i;
    ubyte[4] ub;
    ub[] = cast(ubyte[4]) &i;
    //ub[] = (cast(ubyte*) &i)[0..4];
}
