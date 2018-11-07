// REQUIRED_ARGS: -d
/*
TEST_OUTPUT:
---
fail_compilation/fail12047.d(15): Error: undefined identifier `asdf`
fail_compilation/fail12047.d(16): Error: undefined identifier `asdf`
fail_compilation/fail12047.d(17): Error: undefined identifier `asdf`
fail_compilation/fail12047.d(18): Error: undefined identifier `asdf`
fail_compilation/fail12047.d(19): Error: undefined identifier `asdf`
fail_compilation/fail12047.d(20): Error: undefined identifier `asdf`
fail_compilation/fail12047.d(21): Error: undefined identifier `asdf`
---
*/

@asdf void func() { }
@asdf int var = 1;
@asdf enum E : int { a }
@asdf struct S {}
@asdf class C {}
@asdf interface I {}
@asdf alias int myint;
