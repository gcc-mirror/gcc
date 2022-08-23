/*
TEST_OUTPUT:
---
fail_compilation/fix19059.d(16): Error: octal digit expected, not `8`
fail_compilation/fix19059.d(16): Error: octal literals larger than 7 are no longer supported
fail_compilation/fix19059.d(17): Error: octal digit expected, not `9`
fail_compilation/fix19059.d(17): Error: octal literals larger than 7 are no longer supported
fail_compilation/fix19059.d(18): Error: octal literals `010` are no longer supported, use `std.conv.octal!"10"` instead
---
 */

// https://issues.dlang.org/show_bug.cgi?id=19059

void foo()
{
    auto a = 08;
    auto b = 09;
    auto c = 010;
}
