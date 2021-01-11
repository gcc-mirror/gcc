/*
EXTRA_FILES: imports/fail356.d
TEST_OUTPUT:
---
fail_compilation/fail356b.d(9): Error: variable `fail356b.bar` conflicts with alias `fail356b.bar` at fail_compilation/fail356b.d(8)
---
*/
import imports.fail356 : bar;
int bar; // collides with selective import
