/*
TEST_OUTPUT:
---
fail_compilation/failsafec.d(13): Error: `@safe` function `failsafec.callingsystem` cannot call `@system` delegate `sysdelegate`
---
*/

void delegate() @system sysdelegate;

@safe
void callingsystem()
{
    sysdelegate();
}
