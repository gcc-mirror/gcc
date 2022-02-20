// REQUIRED_ARGS: -c
// EXTRA_FILES: imports/foo10727a.d imports/stdtraits10727.d
/*
TEST_OUTPUT:
---
fail_compilation/imports/foo10727a.d(34): Error: undefined identifier `Frop`
fail_compilation/imports/foo10727a.d(26): Error: template instance `foo10727a.CirBuff!(Foo)` error instantiating
fail_compilation/imports/foo10727a.d(31):        instantiated from here: `Bar!(Foo)`
---
*/

import imports.foo10727a;
