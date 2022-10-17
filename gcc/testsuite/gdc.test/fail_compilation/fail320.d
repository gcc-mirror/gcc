/*
EXTRA_FILES: imports/fail320a.d imports/fail320b.d
TEST_OUTPUT:
---
fail_compilation/fail320.d(11): Error: no overload matches for `foo`
---
*/

import imports.fail320a;
import imports.fail320b;
void main() { foo(); }
