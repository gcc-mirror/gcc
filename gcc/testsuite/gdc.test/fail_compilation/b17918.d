/*
EXTRA_FILES: imports/b17918a.d
TEST_OUTPUT:
---
fail_compilation/imports/b17918a.d(7): Error: undefined identifier `_listMap`
---
*/
// https://issues.dlang.org/show_bug.cgi?id=17918
import imports.b17918a;

class Derived : Base
{
}
