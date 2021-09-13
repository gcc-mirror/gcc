/*
TEST_OUTPUT:
---
fail_compilation/fail239.d(8): Error: type `F` is not an expression
---
*/
class F { int x; }
alias typeof(F).x b;
