// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:
// EXTRA_SOURCES: extra-files/a14446.d

/*
TEST_OUTPUT:
---
fail_compilation/extra-files/a14446.d(5): Error: module x14446 from file fail_compilation/ice14446.d must be imported with 'import x14446;'
---
*/

module x14446;

struct CDB {}
