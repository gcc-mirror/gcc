/*
TEST_OUTPUT:
---
fail_compilation/protection/subpkg/test2.d(3): Error: protection attribute 'package(protection.subpkg2)' does not bind to one of ancestor packages of module `protection.subpkg.test2`
---
*/
import protection.subpkg.test2;
