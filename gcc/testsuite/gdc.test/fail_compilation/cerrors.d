/* REQUIRED_ARGS: -wi
TEST_OUTPUT:
---
fail_compilation/cerrors.d(13): Error: C preprocessor directive `#if` is not supported, use `version` or `static if`
fail_compilation/cerrors.d(13): Error: declaration expected, not `#`
fail_compilation/cerrors.d(17): Error: C preprocessor directive `#endif` is not supported
fail_compilation/cerrors.d(17): Error: declaration expected, not `#`
fail_compilation/cerrors.d(21): Error: token string requires valid D tokens, not `#if`
fail_compilation/cerrors.d(22): Deprecation: token string requires valid D tokens, not `#include`
---
*/

#if 1

void test(wchar_t u);

#endif

// https://issues.dlang.org/show_bug.cgi?id=23792
enum s1 = q{
#if 1
#include <test>
};
