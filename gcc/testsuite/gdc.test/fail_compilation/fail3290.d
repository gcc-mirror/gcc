// https://issues.dlang.org/show_bug.cgi?id=3290
/*
TEST_OUTPUT:
---
fail_compilation/fail3290.d(12): Error: argument type mismatch, `const(int)` to `ref int`
---
*/

void main()
{
    const(int)[] array;
    foreach (ref int i; array) {
        //i = 42;
    }
}
