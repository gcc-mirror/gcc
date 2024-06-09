/*
TEST_OUTPUT:
---
fail_compilation/operator_undefined.d(19): Error: operator `-` is not defined for `toJson(2)` of type `Json`
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
