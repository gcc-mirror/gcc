/*
TEST_OUTPUT:
---
fail_compilation/operator_undefined.d(20): Error: operator `-` is not defined for `Json`
fail_compilation/operator_undefined.d(11):        perhaps overload the operator with `auto opUnary(string op : "-")() {}`
---
*/

import std.stdio;

struct Json
{
    //int opUnary(string op : "-")();
}

Json toJson(int);

void main()
{
    auto x = -2.toJson;
}
