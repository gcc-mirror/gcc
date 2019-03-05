// REQUIRED_ARGS: -Ifail_compilation/extra-files
// EXTRA_SOURCES: extra-files/bar11453.d extra-files/foo11453.d
/*
TEST_OUTPUT
---
fail_compilation/extra-files/foo11453.d(1): Error: module foo11453 from file fail_compilation/extra-files/foo11453.d conflicts with package name foo11453
---
*/

void main() {}
