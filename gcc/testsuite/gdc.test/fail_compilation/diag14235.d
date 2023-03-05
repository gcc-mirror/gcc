/*
EXTRA_FILES: imports/a14235.d
TEST_OUTPUT:
---
fail_compilation/diag14235.d(12): Error: undefined identifier `Undefined` in module `imports.a14235`
fail_compilation/diag14235.d(13): Error: undefined identifier `Something` in module `imports.a14235`, did you mean struct `SomeThing(T...)`?
fail_compilation/diag14235.d(14): Error: `SomeClass` isn't a template
---
*/

import imports.a14235;
imports.a14235.Undefined!Object a;
imports.a14235.Something!Object b;
imports.a14235.SomeClass!Object c;
