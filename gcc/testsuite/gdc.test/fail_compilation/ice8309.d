/*
TEST_OUTPUT:
---
fail_compilation/ice8309.d(10): Error: incompatible types for `(() => 1.0) : (() => 1)`: `double function() pure nothrow @nogc @safe` and `int function() pure nothrow @nogc @safe`
---
*/

void main()
{
    auto x = [()=>1.0, ()=>1];
}
