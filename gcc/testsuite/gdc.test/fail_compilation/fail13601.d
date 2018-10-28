/*
TEST_OUTPUT:
---
fail_compilation/fail13601.d(13): Error: variable `__ctfe` cannot be read at compile time
fail_compilation/fail13601.d(14): Error: variable `__ctfe` cannot be read at compile time
fail_compilation/fail13601.d(15): Error: variable `__ctfe` cannot be read at compile time
fail_compilation/fail13601.d(16): Error: variable __ctfe cannot be read at compile time
---
*/

void test()
{
    static if (__ctfe) {}
    enum a = __ctfe ? "a" : "b";
    static int b = __ctfe * 2;
    int[__ctfe] sarr;
}
