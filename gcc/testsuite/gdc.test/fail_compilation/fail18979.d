// EXTRA_FILES: imports/imp18979.d
/*
TEST_OUTPUT:
---
fail_compilation/fail18979.d(13): Error: no property `__ctor` for `Foo()` of type `imports.imp18979.Foo`
----
*/

import imports.imp18979;

void main()
{
    auto f = Foo(42);
}
