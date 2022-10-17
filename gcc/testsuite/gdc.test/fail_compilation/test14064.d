/*
TEST_OUTPUT:
---
fail_compilation/test14064.d(11): Error: `private` is a keyword, not an `@` attribute
fail_compilation/test14064.d(12): Error: `deprecated` is a keyword, not an `@` attribute
fail_compilation/test14064.d(13): Error: `pure` is a keyword, not an `@` attribute
fail_compilation/test14064.d(14): Error: `nothrow` is a keyword, not an `@` attribute
fail_compilation/test14064.d(15): Error: `in` is a keyword, not an `@` attribute
---
*/
@private int v;
@deprecated void foo();
int goo() @pure;
@nothrow unittest {};
void zoom(@in int x);
