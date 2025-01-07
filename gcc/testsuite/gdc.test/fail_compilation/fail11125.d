/*
TEST_OUTPUT:
---
fail_compilation/fail11125.d(26): Error: template instance `fail11125.filter!(function (int a) pure nothrow @nogc @safe => a + 1)` does not match template declaration `filter(alias predfun)`
  with `predfun = __lambda_L26_C13`
  must satisfy the following constraint:
`       is(ReturnType!predfun == bool)`
fail_compilation/fail11125.d(27): Error: template instance `fail11125.filter!(function (int a) pure nothrow @nogc @safe => a + 1)` does not match template declaration `filter(alias predfun)`
  with `predfun = __lambda_L27_C17`
  must satisfy the following constraint:
`       is(ReturnType!predfun == bool)`
---
*/

template ReturnType(alias fun) { alias int ReturnType; }

template filter(alias predfun)
    if (is(ReturnType!predfun == bool))
{
    static assert(is(ReturnType!predfun == bool));
    auto filter(Range)(Range r) { }
}

void main()
{
    filter!((int a) => a + 1)([1]);  // fails in constraint
    [1].filter!((int a) => a + 1);   // fails internally in static assert!
}
