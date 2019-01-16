// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/diag15411.d(13): Error: function diag15411.test15411.__funcliteral1 cannot access frame of function diag15411.test15411
fail_compilation/diag15411.d(14): Error: function diag15411.test15411.__funcliteral2 cannot access frame of function diag15411.test15411
---
*/

void test15411()
{
    auto i = 0;
    auto j = (function() { return i; })();
    auto f =  function() { return i; };
}
