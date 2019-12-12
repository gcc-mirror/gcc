/*
TEST_OUTPUT:
---
fail_compilation/fail123.d(11): Error: undefined identifier `type`
fail_compilation/fail123.d(17): Error: enum fail123.foo2 base type must not be void
---
*/

// Issue 355 - ICE from enum : nonexistent type

enum foo : type
{
    blah1,
    blah2
}

enum foo2 : void { a, b }
