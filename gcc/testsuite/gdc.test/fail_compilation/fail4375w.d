// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375w.d(15): Warning: else is dangling, add { } after condition at fail_compilation/fail4375w.d(13)
Error: warnings are treated as errors
       Use -wi if you wish to treat warnings only as informational.
---
*/

static if (true)
    version (B)
        struct G1 {}
else
    struct G2 {}

