/*
TEST_OUTPUT:
---
fail_compilation/fail11746.d(18): Error: cannot implicitly convert expression `1` of type `int` to `string`
fail_compilation/fail11746.d(25): Error: cannot implicitly convert expression `1` of type `int` to `string`
fail_compilation/fail11746.d(26): Error: cannot implicitly convert expression `2` of type `int` to `string`
---
*/

string bb(T, U)(T x, U y)
{
    return "3";
}

enum E1
{
    foo = bb(bar, baz),
    bar = 1,
    baz = "2",
}

enum E2
{
    foo = bb(bar, baz),
    bar = 1,
    baz = 2
}
