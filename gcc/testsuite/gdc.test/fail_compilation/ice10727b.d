// REQUIRED_ARGS: -c
// EXTRA_FILES: imports/foo10727b.d imports/stdtraits10727.d
/*
TEST_OUTPUT:
---
fail_compilation/imports/foo10727b.d(25): Error: undefined identifier `Frop`
---
*/

import imports.foo10727b;
