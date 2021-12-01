// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/diag15411.d(17): Error: function `diag15411.test15411.__funcliteral2` cannot access variable `i` in frame of function `diag15411.test15411`
fail_compilation/diag15411.d(16):        `i` declared here
fail_compilation/diag15411.d(18): Error: function `diag15411.test15411.__funcliteral4` cannot access variable `i` in frame of function `diag15411.test15411`
fail_compilation/diag15411.d(16):        `i` declared here
fail_compilation/diag15411.d(26): Error: `static` function `diag15411.testNestedFunction.myFunc2` cannot access function `myFunc1` in frame of function `diag15411.testNestedFunction`
fail_compilation/diag15411.d(25):        `myFunc1` declared here
---
*/

void test15411()
{
    auto i = 0;
    auto j = (function() { return i; })();
    auto f =  function() { return i; };
}

void testNestedFunction ()
{
    int i = 42;

    void myFunc1() { assert(i == 42); }
    static void myFunc2 () { myFunc1(); }
}
