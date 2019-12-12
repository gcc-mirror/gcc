/*
TEST_OUTPUT:
---
fail_compilation/fail50.d(12): Error: need 'this' for address of a
fail_compilation/fail50.d(12): Error: variable a cannot be read at compile time
---
*/

struct Marko
{
    int a;
    int* m = &a;
}
