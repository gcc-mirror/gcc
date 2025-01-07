/*
TEST_OUTPUT:
---
fail_compilation/fail39.d(12): Error: function `fail39.main.__funcliteral_L12_C27` cannot access function `foo` in frame of function `D main`
fail_compilation/fail39.d(11):        `foo` declared here
---
*/

void main()
{
    void foo() {}
    void function() bar = function void() { foo(); };
}
