/*
TEST_OUTPUT:
---
fail_compilation/test22361.d(11): Error: unable to read module `this_module_does_not_exist`
fail_compilation/test22361.d(11):        Expected 'this_module_does_not_exist.d' or 'this_module_does_not_exist/package.d' in one of the following import paths:
import path[0] = fail_compilation
import path[1] = $p:druntime/import$
import path[2] = $p:phobos$
---
*/
import this_module_does_not_exist;
