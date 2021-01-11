/*
EXTRA_FILES: test5412c2.di imports/test5412a.d
TEST_OUTPUT:
---
fail_compilation/test5412c.d(11): Error: import `test5412c.test5412c2` conflicts with import `test5412c.test5412c2` at fail_compilation/test5412c.d(10)
---
*/
module test5412c;

import test5412c2 = imports.test5412a;
import test5412c2;
