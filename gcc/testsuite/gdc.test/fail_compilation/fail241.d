/*
TEST_OUTPUT:
---
fail_compilation/fail241.d(16): Error: mutable method fail241.Foo.f is not callable using a const object
fail_compilation/fail241.d(17): Error: mutable method fail241.Foo.g is not callable using a const object
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
