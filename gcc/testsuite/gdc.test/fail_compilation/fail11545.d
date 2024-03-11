/*
TEST_OUTPUT:
---
fail_compilation/fail11545.d(14): Error: accessing non-static variable `x` requires an instance of `C`
fail_compilation/fail11545.d(18): Error: accessing non-static variable `x` requires an instance of `C`
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
