/*
TEST_OUTPUT:
---
fail_compilation/diag10089.d(15): Error: undefined identifier `chunks` in package `imports`
fail_compilation/diag10089.d(17): Error: template Foo() does not have property 'chunks'
---
*/

import imports.diag10089a, imports.diag10089b;

template Foo() {}

void main()
{
    imports.chunks("abcdef", 2);

    Foo.chunks("abcdef", 2);
}
