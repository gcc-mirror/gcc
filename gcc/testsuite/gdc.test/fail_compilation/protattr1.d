/*
EXTRA_FILES: protection/subpkg/test1.d
TEST_OUTPUT:
---
fail_compilation/protection/subpkg/test1.d(3): Error: visibility attribute `package(undefined)` does not bind to one of ancestor packages of module `protection.subpkg.test1`
---
*/
import protection.subpkg.test1;
