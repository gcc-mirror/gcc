// https://issues.dlang.org/show_bug.cgi?id=14997

/*
TEST_OUTPUT:
---
fail_compilation/fail14997.d(19): Error: none of the overloads of `this` are callable using argument types `()`
fail_compilation/fail14997.d(14):        Candidates are: `fail14997.Foo.this(int a)`
fail_compilation/fail14997.d(15):                        `fail14997.Foo.this(string a)`
---
*/

class Foo
{
    this (int a) {}
    this (string a) {}
}
void main()
{
    auto a = new Foo;
}
