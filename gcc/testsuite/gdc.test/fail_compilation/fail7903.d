/*
TEST_OUTPUT:
---
fail_compilation/fail7903.d(21): Error: variable `fail7903.F1.x` Field members of a `synchronized` class cannot be `public`
fail_compilation/fail7903.d(22): Error: variable `fail7903.F1.y` Field members of a `synchronized` class cannot be `export`
fail_compilation/fail7903.d(27): Error: variable `fail7903.F2.x` Field members of a `synchronized` class cannot be `public`
---
*/
synchronized class K1
{
    public struct S { }
}

synchronized class K2
{
    struct S { }
}

synchronized class F1
{
    public int x;
    export int y;
}

synchronized class F2
{
    int x;
}
