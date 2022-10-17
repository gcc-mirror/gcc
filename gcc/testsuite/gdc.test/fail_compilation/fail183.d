/*
TEST_OUTPUT:
---
fail_compilation/fail183.d(17): Error: attribute `const` is redundant with previously-applied `in`
fail_compilation/fail183.d(18): Error: attribute `scope` cannot be applied with `in`, use `-preview=in` instead
fail_compilation/fail183.d(19): Error: attribute `const` is redundant with previously-applied `in`
fail_compilation/fail183.d(19): Error: attribute `scope` cannot be applied with `in`, use `-preview=in` instead
fail_compilation/fail183.d(20): Error: attribute `scope` cannot be applied with `in`, use `-preview=in` instead
fail_compilation/fail183.d(20): Error: attribute `const` is redundant with previously-applied `in`
fail_compilation/fail183.d(22): Error: attribute `in` cannot be added after `const`: remove `const`
fail_compilation/fail183.d(23): Error: attribute `in` cannot be added after `scope`: remove `scope` and use `-preview=in`
fail_compilation/fail183.d(24): Error: attribute `in` cannot be added after `const`: remove `const`
fail_compilation/fail183.d(25): Error: attribute `in` cannot be added after `const`: remove `const`
---
*/

void f1(in const int x) {}
void f2(in scope int x) {}
void f3(in const scope int x) {}
void f4(in scope const int x) {}

void f5(const in int x) {}
void f6(scope in int x) {}
void f7(const scope in int x) {}
void f8(scope const in int x) {}
