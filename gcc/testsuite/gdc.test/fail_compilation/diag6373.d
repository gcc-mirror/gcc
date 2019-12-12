/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/diag6373.d(15): Error: class diag6373.Bar use of `diag6373.Foo.method(double x)` is hidden by `Bar`; use `alias method = Foo.method;` to introduce base class overload set
---
*/

class Foo
{
    void method(int x) { }
    void method(double x) { }
}

class Bar : Foo
{
    override void method(int x) { }
}

void main() { }
