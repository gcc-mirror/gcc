/*
TEST_OUTPUT:
---
fail_compilation/diag14235.d(11): Error: template identifier 'Undefined' is not a member of module 'imports.a14235'
fail_compilation/diag14235.d(12): Error: template identifier 'Something' is not a member of module 'imports.a14235', did you mean struct 'SomeThing(T...)'?
fail_compilation/diag14235.d(13): Error: imports.a14235.SomeClass is not a template, it is a class
---
*/

import imports.a14235;
imports.a14235.Undefined!Object a;
imports.a14235.Something!Object b;
imports.a14235.SomeClass!Object c;
