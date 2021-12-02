/*
TEST_OUTPUT:
---
fail_compilation/fail19744.d(8): Error: Top-level function `test` has no `this` to which `return` can apply
---
*/

int* test(return scope int* n) return
{
    return n;
}
