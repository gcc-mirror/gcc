// REQUIRED_ARGS: -w
// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375v.d(13): Warning: else is dangling, add { } after condition at fail_compilation/fail4375v.d(11)
---
*/

version (A)
    version (B)
        struct G3 {}
else
    struct G4 {}

