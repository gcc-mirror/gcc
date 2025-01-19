/*
TEST_OUTPUT:
---
fail_compilation/ice8309.d(10): Error: incompatible types for `(__lambda_L10_C15) : (__lambda_L10_C24)`: `double function() pure nothrow @nogc @safe` and `int function() pure nothrow @nogc @safe`
---
*/

void main()
{
    auto x = [()=>1.0, ()=>1];
}
