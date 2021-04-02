/*
TEST_OUTPUT:
---
fail_compilation/failsafeb.d(13): Error: `@safe` function `failsafeb.callingsystem` cannot call `@system` function pointer `sysfuncptr`
---
*/

void function() @system sysfuncptr;

@safe
void callingsystem()
{
    sysfuncptr();
}
