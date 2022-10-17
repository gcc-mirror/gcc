/*
TEST_OUTPUT:
---
fail_compilation/fail13498.d(11): Error: cannot implicitly convert expression `"foo"` of type `string` to `int`
fail_compilation/fail13498.d(16): Error: template instance `fail13498.foo!()` error instantiating
---
*/

int foo()()
{
    return "foo"; // should fail as well
}

void main()
{
    foo();
}
