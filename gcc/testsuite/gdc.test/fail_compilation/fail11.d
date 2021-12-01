/*
TEST_OUTPUT:
---
fail_compilation/fail11.d(12): Error: `int*` has no effect
---
*/

// http://forum.dlang.org/thread/c738o9$1p7i$1@digitaldaemon.com

void main()
{
    TFoo!(int).t; // should produce a "no identifier" error.
}
template TFoo(T) { alias T* t; }
