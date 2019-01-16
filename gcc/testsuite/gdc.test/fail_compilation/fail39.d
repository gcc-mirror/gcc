/*
TEST_OUTPUT:
---
fail_compilation/fail39.d(11): Error: function fail39.main.__funcliteral2 cannot access frame of function D main
---
*/

void main()
{
    void foo() {}
    void function() bar = function void() { foo(); };
}
