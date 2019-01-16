/*
TEST_OUTPUT:
---
fail_compilation/diag9961.d(11): Error: cannot implicitly convert expression `""` of type `string` to `int`
fail_compilation/diag9961.d(14): Error: template instance diag9961.foo!int error instantiating
fail_compilation/diag9961.d(11): Error: cannot implicitly convert expression `""` of type `string` to `int`
fail_compilation/diag9961.d(15): Error: template instance diag9961.foo!char error instantiating
---
*/

void foo(T)(T) { int x = ""; }
void main()
{
    100.foo();
    'a'.foo;
}
