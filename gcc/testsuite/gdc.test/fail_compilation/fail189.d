/*
TEST_OUTPUT:
---
fail_compilation/fail189.d(10): Error: undefined identifier `foo`
---
*/

void bar()
{
    foo(); // should fail
}

version(none):
void foo() {}
