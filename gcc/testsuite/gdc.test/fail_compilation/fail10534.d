/*
TEST_OUTPUT:
---
fail_compilation/fail10534.d(28): Error: illegal operator `+` for `a` of type `int delegate()`
fail_compilation/fail10534.d(28): Error: illegal operator `+` for `b` of type `int delegate()`
fail_compilation/fail10534.d(29): Error: illegal operator `-` for `a` of type `int delegate()`
fail_compilation/fail10534.d(29): Error: illegal operator `-` for `b` of type `int delegate()`
fail_compilation/fail10534.d(30): Error: illegal operator `/` for `a` of type `int delegate()`
fail_compilation/fail10534.d(30): Error: illegal operator `/` for `b` of type `int delegate()`
fail_compilation/fail10534.d(31): Error: illegal operator `*` for `a` of type `int delegate()`
fail_compilation/fail10534.d(31): Error: illegal operator `*` for `b` of type `int delegate()`
fail_compilation/fail10534.d(36): Error: illegal operator `+` for `a` of type `int function()`
fail_compilation/fail10534.d(36): Error: illegal operator `+` for `b` of type `int function()`
fail_compilation/fail10534.d(37): Error: illegal operator `-` for `a` of type `int function()`
fail_compilation/fail10534.d(37): Error: illegal operator `-` for `b` of type `int function()`
fail_compilation/fail10534.d(38): Error: illegal operator `/` for `a` of type `int function()`
fail_compilation/fail10534.d(38): Error: illegal operator `/` for `b` of type `int function()`
fail_compilation/fail10534.d(39): Error: illegal operator `*` for `a` of type `int function()`
fail_compilation/fail10534.d(39): Error: illegal operator `*` for `b` of type `int function()`
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
