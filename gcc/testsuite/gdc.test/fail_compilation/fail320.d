/*
TEST_OUTPUT:
---
fail_compilation/fail320.d(10): Error: no overload matches for foo
---
*/

import imports.fail320a;
import imports.fail320b;
void main() { foo(); }
