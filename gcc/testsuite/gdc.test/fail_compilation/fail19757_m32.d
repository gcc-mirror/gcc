// REQUIRED_ARGS: -m32
/*
TEST_OUTPUT:
---
fail_compilation/fail19757_m32.d(9): Error: cannot implicitly convert expression `"oops"` of type `string` to `uint`
---
*/

auto s = new string("oops");
