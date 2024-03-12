/*
TEST_OUTPUT:
---
fail_compilation/imports/test21164d.d(3): Error: (expression) expected following `static if`
fail_compilation/imports/test21164d.d(3): Error: found `}` instead of statement
fail_compilation/imports/test21164a.d(5): Error: undefined identifier `I`
fail_compilation/test21164.d(12): Error: template instance `test21164a.D!(R!(O(), 1))` error instantiating
---
*/
import imports.test21164a;
import imports.test21164b;
auto GB(D!Q)
{
}
