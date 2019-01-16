// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

/*
TEST_OUTPUT:
---
fail_compilation/imports/diag9210b.d(6): Error: undefined identifier `A`
---
*/

import imports.diag9210b;
interface A {}
