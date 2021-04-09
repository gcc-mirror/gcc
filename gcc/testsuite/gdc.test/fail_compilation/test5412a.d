/*
EXTRA_FILES: imports/test5412a.d imports/test5412b.d
TEST_OUTPUT:
---
fail_compilation/test5412a.d(11): Error: import `test5412a.A` conflicts with import `test5412a.A` at fail_compilation/test5412a.d(10)
---
*/
module test5412a;

import A = imports.test5412a;
import A = imports.test5412b;
