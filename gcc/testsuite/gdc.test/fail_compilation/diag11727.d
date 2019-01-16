/*
TEST_OUTPUT:
---
fail_compilation/diag11727.d(10): Error: type n is not an expression
---
*/
auto returnEnum()
{
    enum n;
    return n;
}
void main()
{
    assert(returnEnum() == 0);
}

/*
TEST_OUTPUT:
---
fail_compilation/diag11727.d(26): Error: type void is not an expression
---
*/
auto returnVoid()
{
    alias v = void;
    return v;
}

/*
TEST_OUTPUT:
---
fail_compilation/diag11727.d(38): Error: template t() has no type
---
*/
auto returnTemplate()
{
    template t() {}
    return t;
}
