// REQUIRED_ARGS:
/*
TEST_OUTPUT:
---
fail_compilation/checkimports2b.d(18): Error: static assert:  `0 == 2` is false
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
