/*
TEST_OUTPUT:
---
fail_compilation/diag10327.d(11): Error: module `test10327` is in file 'imports/test10327.d' which cannot be read
import path[0] = fail_compilation
import path[1] = $p:druntime/import$
import path[2] = $p:phobos$
---
*/

import imports.test10327;  // package.d missing
