// REQUIRED_ARGS: -transition=import -transition=checkimports -de
/*
TEST_OUTPUT:
---
fail_compilation/checkimports2b.d(27): Deprecation: local import search method found variable imports.imp2.X instead of variable checkimports2b.X
fail_compilation/checkimports2b.d(27):        while evaluating: `static assert(2 == 2)`
fail_compilation/checkimports2b.d(33): Deprecation: local import search method found variable imports.imp2.X instead of nothing
fail_compilation/checkimports2b.d(33):        while evaluating: `static assert(2 == 2)`
fail_compilation/checkimports2b.d(34): Deprecation: local import search method found variable imports.imp2.Y instead of nothing
fail_compilation/checkimports2b.d(34):        while evaluating: `static assert(2 == 2)`
fail_compilation/checkimports2b.d(36): Deprecation: local import search method found variable imports.imp2.X instead of variable checkimports2b.X
fail_compilation/checkimports2b.d(36):        while evaluating: `static assert(2 == 2)`
fail_compilation/checkimports2b.d(37): Deprecation: local import search method found variable imports.imp2.Y instead of variable imports.imp1.Y
fail_compilation/checkimports2b.d(37):        while evaluating: `static assert(2 == 2)`
---
*/

// old lookup + information

import imports.imp1;

enum X = 0;

class B
{
    import imports.imp2;
    static assert(X == 2);      // imp2.X --> .X (information)
    int[Y] aa;                  // imp2.Y
}

class C : B
{
    static assert(B.X == 2);    // imp2.X --> error (keep old lookup rule)
    static assert(B.Y == 2);    // imp2.Y --> error (keep old lookup rule)

    static assert(X == 2);      // imp2.X --> .X (information)
    static assert(Y == 2);      // imp2.Y --> imp1.Y (information)
}
