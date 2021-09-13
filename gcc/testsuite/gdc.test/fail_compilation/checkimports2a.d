// REQUIRED_ARGS:
/*
TEST_OUTPUT:
---
fail_compilation/checkimports2a.d(27): Error: no property `X` for type `checkimports2a.B`, did you mean `imports.imp2.X`?
fail_compilation/checkimports2a.d(27):        while evaluating: `static assert((B).X == 0)`
fail_compilation/checkimports2a.d(28): Error: no property `Y` for type `checkimports2a.B`, did you mean `imports.imp2.Y`?
fail_compilation/checkimports2a.d(28):        while evaluating: `static assert((B).Y == 2)`
---
*/

// new lookup + information

import imports.imp1;

enum X = 0;

class B
{
    import imports.imp2;
    static assert(X == 0);      // imp2.X --> .X
    int[Y] aa;                  // imp2.Y
}

class C : B
{
    static assert(B.X == 0);    // imp2.X --> error
    static assert(B.Y == 2);    // imp2.Y --> error

    static assert(X == 0);      // imp2.X --> .X
    static assert(Y == 1);      // imp2.Y --> imp1.Y
}
