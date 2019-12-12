/*
TEST_OUTPUT:
---
fail_compilation/diag8318.d(13): Error: function diag8318.Bar8318.foo return type inference is not supported if may override base class function
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

/*
TEST_OUTPUT:
---
fail_compilation/diag8318.d(24): Error: function diag8318.C10021.makeI return type inference is not supported if may override base class function
---
*/
interface I10021 { I10021 makeI(); }
class D10021 : I10021 { D10021 makeI() { return this; } }
class C10021 : I10021 { auto   makeI() { return this; } }

/*
TEST_OUTPUT:
---
fail_compilation/diag8318.d(38): Error: function diag8318.Bar10195.baz return type inference is not supported if may override base class function
---
*/
interface Foo10195
{
    int baz();
}
class Bar10195 : Foo10195
{
    override auto baz() { return 1; }
}

/*
TEST_OUTPUT:
---
fail_compilation/diag8318.d(50): Error: function diag8318.B14173.foo does not override any function
---
*/
class A14173 {}
class B14173 : A14173
{
    override foo() {}
}
