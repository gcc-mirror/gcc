/*
TEST_OUTPUT:
---
fail_compilation/fail101.d(9): Deprecation: use of complex type `creal` is deprecated, use `std.complex.Complex!(real)` instead
fail_compilation/fail101.d(9): Error: cannot implicitly convert expression `1` of type `int` to `creal`
---
*/

creal c = 1;
