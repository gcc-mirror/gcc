/************************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/pragmas.d(103): Error: one boolean expression expected for `pragma(inline)`, not 2
fail_compilation/pragmas.d(108): Error: one boolean expression expected for `pragma(inline)`, not 2
---
*/

#line 100
void test1()
{
    pragma(inline);
    pragma(inline, true, false);
}

void test2()
{
    pragma(inline, true, false);
}

void test3()
{
    pragma(inline, "string"); // works now
}

void test4()
{
    pragma(unrecognized, "string"); // permitted, just ignored
}
