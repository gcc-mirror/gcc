/*
TEST_OUTPUT:
---
fail_compilation/fail19912c.d(7): Error: alias `fail19912c.object` conflicts with import `fail19912c.object` at fail_compilation/fail19912c.d
---
*/
alias object = int;
void fun(string) { }
