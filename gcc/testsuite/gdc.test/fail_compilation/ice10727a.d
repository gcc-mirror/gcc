// REQUIRED_ARGS: -c
// EXTRA_FILES: imports/foo10727a.d imports/stdtraits10727.d
/*
TEST_OUTPUT:
---
fail_compilation/imports/foo10727a.d(34): Error: undefined identifier `Frop`
---
*/

import imports.foo10727a;
