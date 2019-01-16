// REQUIRED_ARGS: -o-
// EXTRA_SOURCES: imports/a13465.d
/*
TEST_OUTPUT:
---
fail_compilation/imports/a13465.d(10): Error: cannot infer type from template instance isMaskField!()
fail_compilation/ice13465a.d(17): Error: template instance imports.a13465.isMatchingMaskField!() error instantiating
---
*/

module ice13465a;

import imports.a13465;

auto createCheckpointMixins()
{
    enum b = isMatchingMaskField!();
}

immutable checkpointMixins = createCheckpointMixins;
