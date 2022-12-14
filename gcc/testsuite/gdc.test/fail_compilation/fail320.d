/*
EXTRA_FILES: imports/fail320a.d imports/fail320b.d
TEST_OUTPUT:
---
fail_compilation/fail320.d(16): Error: no overload matches for `foo("")`
fail_compilation/fail320.d(16):        Candidates are:
fail_compilation/imports/fail320b.d(1):        foo(T)(string)
fail_compilation/imports/fail320b.d(2):        foo(alias a)()
fail_compilation/imports/fail320a.d(1):        foo(int)
fail_compilation/imports/fail320a.d(2):        foo(bool)
---
*/

import imports.fail320a;
import imports.fail320b;
void main() { foo(""); }
