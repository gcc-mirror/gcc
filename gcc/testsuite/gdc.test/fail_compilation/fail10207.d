/*
TEST_OUTPUT:
---
fail_compilation/fail10207.d(7): Error: user defined attributes not allowed for `alias` declarations
---
*/
alias @Safe int __externC;
