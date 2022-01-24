/*
TEST_OUTPUT:
---
fail_compilation/fail11545.d(14): Error: need `this` for `x` of type `int`
fail_compilation/fail11545.d(18): Error: need `this` for `x` of type `int`
---
*/

class C
{
    int x = 42;

    int function() f1 = function() {
        return x;
    };

    int function() f2 = {
        return x;
    };
}
