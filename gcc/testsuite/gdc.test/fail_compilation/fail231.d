/*
TEST_OUTPUT:
---
fail_compilation/fail231.d(15): Error: class `fail231.Derived` cannot implicitly generate a default constructor when base class `fail231.Base` is missing a default constructor
---
*/

// https://issues.dlang.org/show_bug.cgi?id=951
// Missing line number: no constructor provided for a class derived from a class with no default constructor
class Base
{
    this(int x) {}
}

class Derived : Base
{
}
