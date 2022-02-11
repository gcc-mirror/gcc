/*
TEST_OUTPUT:
---
fail_compilation/diag8510.d(10): Error: alias `diag8510.a` conflicts with alias `diag8510.a` at fail_compilation/diag8510.d(9)
fail_compilation/diag8510.d(15): Error: alias `diag8510.S.a` conflicts with alias `diag8510.S.a` at fail_compilation/diag8510.d(14)
---
*/

alias int a;
alias int a;

int g;
struct S {
    alias g a;
    alias g a;
}

