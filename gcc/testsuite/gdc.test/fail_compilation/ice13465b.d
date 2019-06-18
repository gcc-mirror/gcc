// REQUIRED_ARGS: -o-
// EXTRA_SOURCES: imports/b13465.d
/*
TEST_OUTPUT:
---
fail_compilation/imports/b13465.d(10): Error: cannot infer type from template instance `isMaskField!()`
fail_compilation/ice13465b.d(17): Error: template instance `imports.b13465.isMatchingMaskField!()` error instantiating
---
*/

module ice13465b;

import imports.b13465;

auto createCheckpointMixins()
{
    enum b = isMatchingMaskField!();
}

immutable checkpointMixins = createCheckpointMixins;
