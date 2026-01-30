/*
TEST_OUTPUT:
---
fail_compilation/diag13320.d(20): Error: operator `++` not supported for `f` of type `Foo`
fail_compilation/diag13320.d(14):        perhaps implement `auto opUnary(string op : "++")() {}` or `auto opOpAssign(string op : "+")(int) {}`
fail_compilation/diag13320.d(21): Error: expression `f` of type `Foo` does not have a boolean value
fail_compilation/diag13320.d(14):        perhaps add Cast Operator Overloading with `bool opCast(T : bool)() => ...`
fail_compilation/diag13320.d(22): Error: expression `Foo()` of type `E` does not have a boolean value
fail_compilation/diag13320.d(14):        perhaps add Cast Operator Overloading with `bool opCast(T : bool)() => ...`
---
*/


struct Foo {}
enum E : Foo { a = Foo.init }

void main()
{
    Foo f;
    ++f;
    if (f) {}
    assert(E.a);
}
