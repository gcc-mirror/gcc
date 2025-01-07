/* REQUIRED_ARGS: -preview=safer
TEST_OUTPUT:
---
fail_compilation/safer.d(10): Error: `void` initializers for pointers not allowed in safe functions
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
