/*
TEST_OUTPUT:
---
fail_compilation/diag13320.d(14): Error: operator `++` not supported for `f` of type `Foo`
fail_compilation/diag13320.d(9):        perhaps implement `auto opUnary(string op : "++")() {}` or `auto opOpAssign(string op : "+")(int) {}`
---
*/

struct Foo {}

void main()
{
    Foo f;
    ++f;
}
