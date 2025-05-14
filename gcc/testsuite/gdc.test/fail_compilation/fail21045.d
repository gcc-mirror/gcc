/*
TEST_OUTPUT:
---
fail_compilation/fail21045.d(12): Error: unable to read module `__stdin`
fail_compilation/fail21045.d(12):        Expected '__stdin.d' or '__stdin/package.d' in one of the following import paths:
import path[0] = fail_compilation
import path[1] = $p:druntime/import$
import path[2] = $p:phobos$
---
*/

import __stdin;
