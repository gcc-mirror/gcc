/*
TEST_OUTPUT:
---
fail_compilation/fail13498.d(11): Error: return value `"foo"` of type `string` does not match return type `int`, and cannot be implicitly converted
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
