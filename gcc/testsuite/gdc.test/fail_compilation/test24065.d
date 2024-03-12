// https://issues.dlang.org/show_bug.cgi?id=24065

/*
TEST_OUTPUT:
---
fail_compilation/test24065.d(12): Error: string expected as argument of __traits `getTargetInfo` instead of `int`
fail_compilation/test24065.d(15): Error: string expected as argument of __traits `getTargetInfo` instead of `foo`
fail_compilation/test24065.d(18): Error: string expected as argument of __traits `getTargetInfo` instead of `e`
---
*/

auto s1 = __traits(getTargetInfo, int);

void foo() {}
auto s2 = __traits(getTargetInfo, foo);

enum e;
auto s3 = __traits(getTargetInfo, e);
