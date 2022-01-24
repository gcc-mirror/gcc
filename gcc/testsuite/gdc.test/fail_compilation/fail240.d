/*
TEST_OUTPUT:
---
fail_compilation/fail240.d(9): Error: type `F` is not an expression
---
*/

class F { int x; }
alias typeof(typeof(F).x) b;
