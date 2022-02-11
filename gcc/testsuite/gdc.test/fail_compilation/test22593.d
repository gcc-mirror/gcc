// https://issues.dlang.org/show_bug.cgi?id=22593

/*
TEST_OUTPUT:
---
fail_compilation/test22593.d(14): Error: Cannot define both an rvalue constructor and a copy constructor for `struct Foo`
fail_compilation/test22593.d(22):        Template instance `__ctor!(immutable(Foo!int), immutable(Foo!int))` creates a rvalue constructor for `struct Foo`
fail_compilation/test22593.d(22): Error: template instance `test22593.Foo!int.Foo.__ctor!(immutable(Foo!int), immutable(Foo!int))` error instantiating
---
*/

struct Foo(T)
{
    this(Rhs, this This)(scope Rhs rhs){}

    this(ref scope typeof(this) rhs){}
}

void main()
{
    immutable Foo!int a;
    a.__ctor(a);
}
