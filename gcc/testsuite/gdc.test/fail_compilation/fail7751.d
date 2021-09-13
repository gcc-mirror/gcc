/*
TEST_OUTPUT:
---
fail_compilation/fail7751.d(15): Error: no constructor for `Foo`
fail_compilation/fail7751.d(23): Error: template instance `fail7751.foo!int` error instantiating
---
*/
class Foo(T)
{
    T x;
    Foo y;
}
auto foo(T)(T x, Foo!T y=null)
{
    return new Foo!T(x, y);
}
void bar(U)(U foo, U[] spam=[])
{
    spam ~= [];
}
void main()
{
    bar(foo(0));
}
