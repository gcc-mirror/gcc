/*
TEST_OUTPUT:
---
fail_compilation/fail248.d(9): Error: type `int` is not an expression
---
*/

alias int foo;
typeof(foo) a; // ok
