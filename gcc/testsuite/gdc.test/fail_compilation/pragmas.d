/*
REQUIRED_ARGS:
PERMUTE_ARGS:
*/

/************************************************************/

/*
TEST_OUTPUT:
---
fail_compilation/pragmas.d(103): Error: boolean expression expected for pragma(inline)
fail_compilation/pragmas.d(108): Error: boolean expression expected for pragma(inline)
fail_compilation/pragmas.d(113): Error: pragma(inline, true or false) expected, not `"string"`
fail_compilation/pragmas.d(118): Error: unrecognized `pragma(unrecognized)`
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
    pragma(inline, "string");
}

void test4()
{
    pragma(unrecognized, "string");
}


