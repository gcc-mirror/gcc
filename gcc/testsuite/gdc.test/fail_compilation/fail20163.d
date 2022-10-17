// REQUIRED_ARGS: -de
// EXTRA_FILES: imports/fail20164.d
/*
TEST_OUTPUT:
---
fail_compilation/fail20163.d-mixin-11(11): Deprecation: module `imports.fail20164` is deprecated
---
*/
module fail20163;

mixin("import imports.fail20164;");
