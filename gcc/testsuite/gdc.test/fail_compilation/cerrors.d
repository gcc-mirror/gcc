/* REQUIRED_ARGS: -wi
TEST_OUTPUT:
---
fail_compilation/cerrors.d(11): Error: C preprocessor directive `#if` is not supported, use `version` or `static if`
fail_compilation/cerrors.d(11): Error: declaration expected, not `#`
fail_compilation/cerrors.d(15): Warning: C preprocessor directive `#endif` is not supported
fail_compilation/cerrors.d(15): Error: declaration expected, not `#`
---
*/

#if 1

void test(wchar_t u);

#endif
