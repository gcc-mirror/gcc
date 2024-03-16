// REQUIRED_ARGS: -c
// EXTRA_FILES: imports/foo10727b.d imports/stdtraits10727.d
/*
TEST_OUTPUT:
---
fail_compilation/imports/foo10727b.d(25): Error: undefined identifier `Frop`
fail_compilation/imports/foo10727b.d(17): Error: template instance `foo10727b.CirBuff!(Foo)` error instantiating
fail_compilation/imports/foo10727b.d(22):        instantiated from here: `Bar!(Foo)`
---
*/

import imports.foo10727b;
