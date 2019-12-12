/*
TEST_OUTPUT:
---
fail_compilation/fail10534.d(28): Error: 'a' is not of arithmetic type, it is a int delegate()
fail_compilation/fail10534.d(28): Error: 'b' is not of arithmetic type, it is a int delegate()
fail_compilation/fail10534.d(29): Error: 'a' is not of arithmetic type, it is a int delegate()
fail_compilation/fail10534.d(29): Error: 'b' is not of arithmetic type, it is a int delegate()
fail_compilation/fail10534.d(30): Error: 'a' is not of arithmetic type, it is a int delegate()
fail_compilation/fail10534.d(30): Error: 'b' is not of arithmetic type, it is a int delegate()
fail_compilation/fail10534.d(31): Error: 'a' is not of arithmetic type, it is a int delegate()
fail_compilation/fail10534.d(31): Error: 'b' is not of arithmetic type, it is a int delegate()
fail_compilation/fail10534.d(36): Error: 'a' is not of arithmetic type, it is a int function()
fail_compilation/fail10534.d(36): Error: 'b' is not of arithmetic type, it is a int function()
fail_compilation/fail10534.d(37): Error: 'a' is not of arithmetic type, it is a int function()
fail_compilation/fail10534.d(37): Error: 'b' is not of arithmetic type, it is a int function()
fail_compilation/fail10534.d(38): Error: 'a' is not of arithmetic type, it is a int function()
fail_compilation/fail10534.d(38): Error: 'b' is not of arithmetic type, it is a int function()
fail_compilation/fail10534.d(39): Error: 'a' is not of arithmetic type, it is a int function()
fail_compilation/fail10534.d(39): Error: 'b' is not of arithmetic type, it is a int function()
---
*/

void main()
{
    {
        int delegate() a = ()=>5;
        int delegate() b = ()=>5;
        auto c1 = a + b;  // passes (and will crash if c1() called)
        auto c2 = a - b;  // passes (and will crash if c2() called)
        auto c3 = a / b;  // a & b not of arithmetic type
        auto c4 = a * b;  // a & b not of arithmetic type
    }
    {
        int function() a = ()=>5;
        int function() b = ()=>5;
        auto c1 = a + b;
        auto c2 = a - b;
        auto c3 = a / b;
        auto c4 = a * b;
    }
}
