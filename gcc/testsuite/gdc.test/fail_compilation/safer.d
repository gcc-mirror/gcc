/* REQUIRED_ARGS: -preview=safer
TEST_OUTPUT:
---
fail_compilation/safer.d(10): Error: `void` initializing a pointer is not allowed in a function with default safety with `-preview=safer`
---
*/

void test1()
{
    int* p = void;
}

void foo3() { }

void test2()
{
    foo3(); // should not be an error
}
