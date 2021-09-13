/*
EXTRA_FILES: imports/test5412a.d imports/test5412b.d
TEST_OUTPUT:
---
fail_compilation/test5412b.d(11): Error: static import `test5412b.A` conflicts with import `test5412b.A` at fail_compilation/test5412b.d(10)
---
*/
module test5412b;

import A = imports.test5412a;
static import A = imports.test5412b;
