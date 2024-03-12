// EXTRA_FILES: imports/imp18979.d
/*
TEST_OUTPUT:
---
fail_compilation/fail18979.d(14): Error: no property `__ctor` for `Foo()` of type `imports.imp18979.Foo`
fail_compilation/imports/imp18979.d(3):        struct `Foo` defined here
----
*/

import imports.imp18979;

void main()
{
    auto f = Foo(42);
}
