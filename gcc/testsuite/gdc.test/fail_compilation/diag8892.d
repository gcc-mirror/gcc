/*
TEST_OUTPUT:
---
fail_compilation/diag8892.d(14): Error: cannot implicitly convert expression `['A']` of type `char[]` to `char[2]`
---
*/
struct Foo
{
    char[2] data;
}

void main()
{
    auto f = Foo(['A']);
}
