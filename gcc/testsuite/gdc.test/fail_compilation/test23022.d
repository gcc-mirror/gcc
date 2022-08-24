/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test23022.d(14): Error: scope parameter `p` may not be returned
---
*/

// https://issues.dlang.org/show_bug.cgi?id=23022
// Typesafe variadic parameter should not infer return

auto ir(string[] p...)
{
    return p;
}
