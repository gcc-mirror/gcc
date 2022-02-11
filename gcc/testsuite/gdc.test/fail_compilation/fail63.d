/*
TEST_OUTPUT:
---
fail_compilation/fail63.d(11): Error: debug `Foo` defined after use
---
*/

debug (Foo)
    int x;

debug = Foo;
