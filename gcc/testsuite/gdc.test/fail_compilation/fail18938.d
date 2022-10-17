// REQUIRED_ARGS: -c -Ifail_compilation/imports/
// EXTRA_SOURCES: imports/test18938a/cache.d imports/test18938a/file.d
// EXTRA_FILES: imports/test18938b/file.d
/*
TEST_OUTPUT:
---
fail_compilation/imports/test18938b/file.d(20): Error: undefined identifier `No`
---
*/

void main() {}
