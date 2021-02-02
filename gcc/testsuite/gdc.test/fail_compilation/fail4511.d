/*
TEST_OUTPUT:
---
fail_compilation/fail4511.d(18): Error: cannot implicitly override base class method `fail4511.test72.X.func` with `fail4511.test72.Y.func`; add `override` attribute
---
*/
void test72()
{
    class A {}
    class B : A {}

    class X
    {
        abstract A func();
    }
    class Y : X
    {
        B func() { return new A(); }
    }
}

void main() {}
