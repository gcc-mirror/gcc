/*
REQUIRED_ARGS: -betterC
TEST_OUTPUT:
---
fail_compilation/test18312.d(14): Error: array concatenation of expression `"[" ~ s ~ "]"` requires the GC which is not available with -betterC
---
*/

// https://issues.dlang.org/show_bug.cgi?id=18312

extern (C) void main()
{
    scope string s;
    s = "[" ~ s ~ "]";
}
