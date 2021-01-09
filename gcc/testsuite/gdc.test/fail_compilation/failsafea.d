/*
TEST_OUTPUT:
---
fail_compilation/failsafea.d(14): Error: `@safe` function `failsafea.callingsystem` cannot call `@system` function `failsafea.systemfunc`
fail_compilation/failsafea.d(9):        `failsafea.systemfunc` is declared here
---
*/

void systemfunc() @system {}

@safe
void callingsystem()
{
    systemfunc();
}
