/*
TEST_OUTPUT:
---
fail_compilation/ice8742.d(15): Error: class `ice8742.main.__anonclass1` is nested within `main`, but super class `D` is nested within `C`
---
*/
class C
{
    class D { }
}

void main ( )
{
    auto c = new C;
    auto d = c.new class C.D { };
}
