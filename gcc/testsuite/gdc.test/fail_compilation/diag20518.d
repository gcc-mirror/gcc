// EXTRA_FILES: imports/diag20518a.d imports/diag20518a/b.d
/*
TEST_OUTPUT:
---
fail_compilation/diag20518.d(11): Error: module `diag20518a` from file fail_compilation/imports/diag20518a.d conflicts with package `imports.diag20518a`
---
*/

import imports.diag20518a.b;  // from here 'imports.diag20518a' represents a package and you can optionally
                              // import its package.d with 'import imports.diag20518a;', but anyway
import imports.diag20518a;    // if 'imports/diag20518a.d' exists it will conflict with it.
