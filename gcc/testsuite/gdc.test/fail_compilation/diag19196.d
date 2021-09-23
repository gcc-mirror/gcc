/*
TEST_OUTPUT:
---
fail_compilation/diag19196.d(11): Error: unable to determine fields of `B` because of forward references
fail_compilation/diag19196.d(15): Error: template instance `diag19196.Foo!(B)` error instantiating
---
*/
module diag19196;
struct Foo(T)
{
    alias F = typeof(T.tupleof);
}
struct B
{
    Foo!B b;
}
