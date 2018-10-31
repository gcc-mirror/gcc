/*
TEST_OUTPUT:
---
fail_compilation/fail62.d(11): Error: version Foo defined after use
---
*/

version (Foo)
    int x;

version = Foo;
