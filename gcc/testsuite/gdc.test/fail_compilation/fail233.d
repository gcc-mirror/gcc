// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/fail233.d(11): Error: variable fail233.bug1176.v void[1] does not have a default initializer
---
*/

void bug1176()
{
    void[1] v;
}
