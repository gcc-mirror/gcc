// https://issues.dlang.org/show_bug.cgi?id=22121
// EXTRA_FILES: fail22121/imports/test22121/package.d
/*
TEST_OUTPUT:
---
fail_compilation/fail22121/imports/test22121/package.d(1): Error: package name 'fail22121' conflicts with usage as a module name in file fail_compilation/fail22121.d
---
*/

module fail22121;
import fail22121.imports.test22121;
