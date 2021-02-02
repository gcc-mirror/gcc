/*
TEST_OUTPUT:
---
fail_compilation/fail7178.d(10): Error: undefined identifier `contents` in module `fail7178`
fail_compilation/fail7178.d(12): Error: mixin `fail7178.populate!int` error instantiating
---
*/
template populate(overloads...)
{
    mixin populate!(.contents);
}
public mixin populate!int;

