/*
TEST_OUTPUT:
---
fail_compilation/ice11922.d(11): Error: undefined identifier `a`
fail_compilation/ice11922.d(17): Error: template instance ice11922.S.f!int error instantiating
---
*/

struct S
{
    auto f(B)(B) { return a; }
}

void main()
{
    S s;
    s.f(5);
}
