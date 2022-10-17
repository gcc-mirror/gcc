// REQUIRED_ARGS: -de
// EXTRA_FILES: imports/fail20164.d
/*
TEST_OUTPUT:
---
fail_compilation/fail20164.d(13): Deprecation: module `imports.fail20164` is deprecated
---
*/
module fail20164;

void foo()
{
    import imports.fail20164;
}
