/*
TEST_OUTPUT:
---
fail_compilation/b17285.d(14): Error: type `ONE` has no value
fail_compilation/b17285.d(14): Error: type `TWO` has no value
fail_compilation/b17285.d(14): Error: cannot implicitly convert expression `ONE` of type `b17285.ONE` to `int`
---
*/

class ONE {}
enum TWO;

void foo() {
    foreach(key; [ONE, TWO, 1]) {}
}
