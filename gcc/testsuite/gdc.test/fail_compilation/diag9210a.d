// REQUIRED_ARGS: -o-
// EXTRA_FILES: imports/diag9210b.d imports/diag9210c.d imports/diag9210stdcomplex.d imports/diag9210stdtraits.d
/*
TEST_OUTPUT:
---
fail_compilation/imports/diag9210b.d(6): Error: undefined identifier `A`
---
*/

import imports.diag9210b;
interface A {}
