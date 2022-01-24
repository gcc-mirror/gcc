/*
TEST_OUTPUT:
---
fail_compilation/diag8178.d(14): Error: cannot modify manifest constant `s`
---
*/

struct Foo
{
    enum string s = "";
}
void main()
{
    Foo.s = "";
}
