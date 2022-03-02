/*
TEST_OUTPUT:
---
fail_compilation/casttuple.d(104): Error: cannot cast `__tup1_field_0` of type `int` to tuple type `(string)`
fail_compilation/casttuple.d(107): Error: cannot cast `tuple(__tup2_field_0, __tup2_field_1)` of type `(int, int)` to tuple type `(string, string)`
fail_compilation/casttuple.d(111): Error: cannot cast `tuple(foo, 123)` of type `(int, int)` to tuple type `(string, string)`
---
 */

alias tuple(T...) = T;

#line 100

void nomatch()
{
    tuple!int tup1;
    auto x = cast(tuple!string) tup1;

    tuple!(int, int) tup2;
    auto y = cast(tuple!(string, string)) tup2;

    int foo;
    alias tup3 = tuple!(foo, 123);
    auto z = cast(tuple!(string, string)) tup3;
}
