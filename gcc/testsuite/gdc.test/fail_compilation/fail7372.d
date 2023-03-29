/*
TEST_OUTPUT:
---
fail_compilation/imports/fail7372.d(7): Error: undefined identifier `X`
fail_compilation/fail7372.d(4):        parent scope from here: `mixin Issue7372!()`
---
*/
#line 1
import imports.fail7372;
interface I {}
class C : I {
    mixin Issue7372!();
}
