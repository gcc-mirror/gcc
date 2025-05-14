/*
TEST_OUTPUT:
---
fail_compilation/fail10534.d(24): Error: illegal operator `+` for `a` of type `int delegate()`
fail_compilation/fail10534.d(24): Error: illegal operator `+` for `b` of type `int delegate()`
fail_compilation/fail10534.d(25): Error: illegal operator `-` for `a` of type `int delegate()`
fail_compilation/fail10534.d(25): Error: illegal operator `-` for `b` of type `int delegate()`
fail_compilation/fail10534.d(26): Error: illegal operator `/` for `a` of type `int delegate()`
fail_compilation/fail10534.d(27): Error: illegal operator `*` for `a` of type `int delegate()`
fail_compilation/fail10534.d(32): Error: illegal operator `+` for `a` of type `int function()`
fail_compilation/fail10534.d(32): Error: illegal operator `+` for `b` of type `int function()`
fail_compilation/fail10534.d(33): Error: illegal operator `-` for `a` of type `int function()`
fail_compilation/fail10534.d(33): Error: illegal operator `-` for `b` of type `int function()`
fail_compilation/fail10534.d(34): Error: illegal operator `/` for `a` of type `int function()`
fail_compilation/fail10534.d(35): Error: illegal operator `*` for `a` of type `int function()`
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
