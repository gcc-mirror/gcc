// EXTRA_SOURCES: imports/a14116.d
/*
TEST_OUTPUT:
---
fail_compilation/imports/a14116.d(3): Error: module ice14116.ice14116 from file fail_compilation/ice14116.d must be imported with 'import ice14116.ice14116;'
---
*/

module ice14116.ice14116;

void foo() {}
