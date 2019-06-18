/*
TEST_OUTPUT:
---
fail_compilation/fail169.d(8): Error: cannot have `const out` parameter of type `const(int)`
---
*/

void foo(const out int x) { }
