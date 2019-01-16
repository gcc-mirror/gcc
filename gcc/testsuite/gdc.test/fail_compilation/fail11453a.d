// REQUIRED_ARGS: -Ifail_compilation/extra-files
// EXTRA_SOURCES: extra-files/foo11453.d extra-files/bar11453.d
/*
TEST_OUTPUT
---
fail_compilation/extra-files/bar11453.d(1): Error: package name 'foo11453' conflicts with usage as a module name in file fail_compilation/extra-files/foo11453.d
---
*/

void main() {}
