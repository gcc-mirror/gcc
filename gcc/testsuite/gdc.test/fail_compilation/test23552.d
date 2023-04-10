// https://issues.dlang.org/show_bug.cgi?id=23552

/*
TEST_OUTPUT:
---
fail_compilation/test23552.d(17): Error: cannot implicitly override base class method `test23552.Base.foo` with `test23552.Derived.foo`; add `override` attribute
---
*/

abstract class Base
{
    void foo();
}

class Derived : Base
{
    void foo() { }
    int data() { return 0; }
}

class DerivedX : Derived
{
    override int data() { return 1; }
}
