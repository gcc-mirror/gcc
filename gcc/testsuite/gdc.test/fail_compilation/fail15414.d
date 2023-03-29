// https://issues.dlang.org/show_bug.cgi?id=15414

// REQUIRED_ARGS: -de

/*
TEST_OUTPUT:
---
fail_compilation/fail15414.d(20): Deprecation: `__traits(getAttributes)` may only be used for individual functions, not overload sets such as: `fun`
fail_compilation/fail15414.d(20):        the result of `__traits(getOverloads)` may be used to select the desired function to extract attributes from
---
*/

@("gigi")
void fun() {}
@("mimi")
void fun(int) {}

void main()
{
    auto t =  __traits(getAttributes, fun);
}
