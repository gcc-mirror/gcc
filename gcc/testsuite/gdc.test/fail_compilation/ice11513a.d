/*
TEST_OUTPUT:
---
fail_compilation/imports/ice11513x.d(1): Error: package name 'ice11513a' conflicts with usage as a module name in file fail_compilation/ice11513a.d
---
*/

module ice11513a;

import imports.ice11513x;
