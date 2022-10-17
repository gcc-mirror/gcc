/*
TEST_OUTPUT:
---
fail_compilation/fail241.d(18): Error: mutable method `fail241.Foo.f` is not callable using a `const` object
fail_compilation/fail241.d(13):        Consider adding `const` or `inout` here
fail_compilation/fail241.d(19): Error: mutable method `fail241.Foo.g` is not callable using a `const` object
fail_compilation/fail241.d(14):        Consider adding `const` or `inout` here
---
*/

class Foo
{
    public void f() { }
    private void g() { }

    invariant()
    {
        f();  // error, cannot call public member function from invariant
        g();  // ok, g() is not public
    }
}
