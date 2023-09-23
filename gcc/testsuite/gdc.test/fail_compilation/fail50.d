/*
TEST_OUTPUT:
---
fail_compilation/fail50.d(12): Error: taking the address of non-static variable `a` requires an instance of `Marko`
fail_compilation/fail50.d(12): Error: variable `a` cannot be read at compile time
---
*/

struct Marko
{
    int a;
    int* m = &a;
}
