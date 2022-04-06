/*
TEST_OUTPUT:
---
fail_compilation/fail47.d(8): Error: variable `fail47._foo` is aliased to a function
---
*/
void foo() {}
int _foo;
alias _foo foo;

void main()
{
    foo = 1;
}
