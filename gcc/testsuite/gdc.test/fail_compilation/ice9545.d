// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
----
fail_compilation/ice9545.d(14): Error: type `int` has no value
fail_compilation/ice9545.d(14):        perhaps use `int.init`
----
*/

struct S { template T(X) { alias T = X; } }

void main()
{
    auto x1 = S.init.T!int; // ICE
}
