/*
EXTRA_FILES: imports/fail356.d
TEST_OUTPUT:
---
fail_compilation/fail356a.d(9): Error: variable `fail356a.imports` conflicts with import `fail356a.imports` at fail_compilation/fail356a.d(8)
---
*/
import imports.fail356;
int imports; // collides with package name
