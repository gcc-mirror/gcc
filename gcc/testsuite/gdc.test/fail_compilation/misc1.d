/*
TEST_OUTPUT:
---
fail_compilation/misc1.d(108): Error: `5` has no effect
fail_compilation/misc1.d(109): Error: `1 + 2` has no effect
---
*/

#line 100

/***************************************************/
//https://issues.dlang.org/show_bug.cgi?id=12490

void hasSideEffect12490(){}

void issue12490()
{
    5, hasSideEffect12490();
    1 + 2, hasSideEffect12490();
}
