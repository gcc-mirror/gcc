/*
EXTRA_FILES: imports/diag10089a.d imports/diag10089b.d
TEST_OUTPUT:
---
fail_compilation/diag10089.d(16): Error: undefined identifier `chunks` in package `imports`
fail_compilation/diag10089.d(18): Error: template `Foo()` does not have property `chunks`
---
*/

import imports.diag10089a, imports.diag10089b;

template Foo() {}

void main()
{
    imports.chunks("abcdef", 2);

    Foo.chunks("abcdef", 2);
}
