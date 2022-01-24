/*
TEST_OUTPUT:
---
fail_compilation/fail105.d(11): Error: cannot cast `"bar"` to `int` at compile time
---
*/

//int foo = "foo";

// just Access Violation happens.
int bar = cast(int)cast(char*)"bar";
