/*
TEST_OUTPUT:
---
fail_compilation/diag8318.d(18): Error: function `diag8318.Bar8318.foo` return type inference is not supported if may override base class function
fail_compilation/diag8318.d(23): Error: function `diag8318.C10021.makeI` return type inference is not supported if may override base class function
fail_compilation/diag8318.d(31): Error: function `diag8318.Bar10195.baz` return type inference is not supported if may override base class function
fail_compilation/diag8318.d(37): Error: function `diag8318.B14173.foo` does not override any function
fail_compilation/diag8318.d(23): Error: class `diag8318.C10021` interface function `I10021 makeI()` is not implemented
fail_compilation/diag8318.d(29): Error: class `diag8318.Bar10195` interface function `int baz()` is not implemented
---
*/
class Foo8318
{
    auto foo() { return "Foo.foo"; }
}
class Bar8318 : Foo8318
{
    override auto foo() { return "Bar.foo"; }
}

interface I10021 { I10021 makeI(); }
class D10021 : I10021 { D10021 makeI() { return this; } }
class C10021 : I10021 { auto   makeI() { return this; } }

interface Foo10195
{
    int baz();
}
class Bar10195 : Foo10195
{
    override auto baz() { return 1; }
}

class A14173 {}
class B14173 : A14173
{
    override foo() {}
}
