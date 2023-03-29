/*
TEST_OUTPUT:
---
fail_compilation/fail12.d(19): Error: `abc` matches conflicting symbols:
fail_compilation/fail12.d(11):        function `fail12.main.Foo!(y).abc`
fail_compilation/fail12.d(11):        function `fail12.main.Foo!(y).abc`
---
*/
template Foo(alias b)
{
    int abc() { return b; }
}

void main()
{
    int y = 8;
    mixin Foo!(y);
    mixin Foo!(y);
    assert(abc() == 8);
}
