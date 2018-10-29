/*
TEST_OUTPUT:
---
fail_compilation/diag15669.d(14): Error: variable __b_field_0 cannot be read at compile time
---
*/

alias AliasSeq(A ...) = A;

void foo()
{
    AliasSeq!int a;
    AliasSeq!int b;
    a[b];
}
