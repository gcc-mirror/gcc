/*
TEST_OUTPUT:
---
fail_compilation/fail19687.d(17): Error: no property `nonexisting` for `""` of type `string`
---
*/

struct S
{
    void opDispatch(string name)() {}
    void opDispatch(string name)(string value) {}
}

void main()
{
    S n;
    n.foo = "".nonexisting();
}
