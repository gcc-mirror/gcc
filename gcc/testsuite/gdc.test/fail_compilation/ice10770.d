/*
TEST_OUTPUT:
---
fail_compilation/ice10770.d(13): Error: enum ice10770.E2 is forward referenced looking for base type
fail_compilation/ice10770.d(13):        while evaluating: `static assert(is(E2 e == enum))`
---
*/

enum E1 : int;
static assert(is(E1 e == enum) && is(e == int));

enum E2;
static assert(is(E2 e == enum));
