/*
TEST_OUTPUT:
---
fail_compilation/fail9790.d(13): Error: undefined identifier `_Unused_`
fail_compilation/fail9790.d(20): Error: template instance `fail9790.foo!()` error instantiating
fail_compilation/fail9790.d(18): Error: undefined identifier `_Unused_`
fail_compilation/fail9790.d(21): Error: template instance `fail9790.bar!()` error instantiating
---
*/

template foo()
{
    enum bool _foo = _Unused_._unused_;
    enum bool foo = _foo;
}
template bar()
{
    enum bool bar = _Unused_._unused_;
}
alias Foo = foo!();
alias Bar = bar!();
