// https://issues.dlang.org/show_bug.cgi?id=4375: Dangling else
/*
TEST_OUTPUT:
---
fail_compilation/fail4375u.d(12): Error: else is dangling, add { } after condition at fail_compilation/fail4375u.d(10)
---
*/

static if (true)
    static if (false)
        struct G1 {}
else
    struct G2 {}
