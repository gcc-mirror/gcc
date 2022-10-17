/*
EXTRA_FILES: imports/ice11513y.d
TEST_OUTPUT:
---
fail_compilation/imports/ice11513y.d(1): Error: package name 'ice11513b' conflicts with usage as a module name in file fail_compilation/ice11513b.d
---
*/

module ice11513b;

import imports.ice11513y;
