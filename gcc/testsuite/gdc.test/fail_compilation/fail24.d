/*
TEST_OUTPUT:
---
fail_compilation/fail24.d(11): Error: alias `fail24.strtype` conflicts with alias `fail24.strtype` at fail_compilation/fail24.d(10)
fail_compilation/fail24.d(12): Error: alias `fail24.strtype` conflicts with alias `fail24.strtype` at fail_compilation/fail24.d(11)
fail_compilation/fail24.d(13): Error: alias `fail24.strtype` conflicts with alias `fail24.strtype` at fail_compilation/fail24.d(12)
---
*/

alias char[]  strtype;
alias char[64] strtype;
alias char[128] strtype;
alias char[256] strtype;

int main()
{
    printf("%u", strtype.sizeof);
    return 0;
}
