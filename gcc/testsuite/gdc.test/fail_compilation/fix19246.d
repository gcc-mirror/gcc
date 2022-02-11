/*
TEST_OUTPUT:
---
fail_compilation/fix19246.d(15): Error: `0b_` isn't a valid integer literal, use `0b0` instead
fail_compilation/fix19246.d(16): Error: `0B_` isn't a valid integer literal, use `0B0` instead
fail_compilation/fix19246.d(17): Error: `0b` isn't a valid integer literal, use `0b0` instead
fail_compilation/fix19246.d(18): Error: `0B` isn't a valid integer literal, use `0B0` instead
---
 */

// https://issues.dlang.org/show_bug.cgi?id=19246

void foo()
{
    auto a = 0b_;
    auto b = 0B_;
    auto c = 0b;
    auto d = 0B;
}
