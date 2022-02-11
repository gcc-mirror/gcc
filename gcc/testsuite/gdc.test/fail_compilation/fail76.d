/*
TEST_OUTPUT:
---
fail_compilation/fail76.d(9): Error: alias `fail76.a` conflicts with alias `fail76.a` at fail_compilation/fail76.d(8)
---
*/

alias main a;
alias void a;

void main()
{
    a;
}
