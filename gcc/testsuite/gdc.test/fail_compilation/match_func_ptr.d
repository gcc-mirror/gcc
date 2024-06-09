/*
TEST_OUTPUT:
---
fail_compilation/match_func_ptr.d(13): Error: cannot match delegate literal to function pointer type `void function()`
fail_compilation/match_func_ptr.d(14): Error: cannot match function literal to delegate type `void delegate()`
fail_compilation/match_func_ptr.d(15): Error: cannot infer parameter types from `int function()`
fail_compilation/match_func_ptr.d(16): Error: cannot infer parameter types from `int delegate(int, int)`
---
*/

void main()
{
    void function() f = delegate {};
    void delegate() d = function {};
    int function() f2 = i => 2;
    int delegate(int, int) d2 = i => 2;
}
