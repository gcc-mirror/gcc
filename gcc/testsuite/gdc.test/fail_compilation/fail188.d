/*
TEST_OUTPUT:
---
fail_compilation/fail188.d(15): Error: function fail188.Derived.foo cannot override final function fail188.Base.foo
---
*/

class Base
{
    final void foo() {}
}

class Derived : Base
{
    void foo() {}
}
