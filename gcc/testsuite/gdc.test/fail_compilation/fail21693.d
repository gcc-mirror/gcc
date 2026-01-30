/*
TEST_OUTPUT:
---
fail_compilation/fail21693.d(15): Error: function `fail` is not callable using argument types `(uint)`
fail_compilation/fail21693.d(15):        cannot pass rvalue argument `__rvalue(s.x)` of type `uint` to parameter `ref uint`
fail_compilation/fail21693.d(11):        `fail21693.fail(ref uint)` declared here
fail_compilation/fail21693.d(16): Error: rvalue `__rvalue(s.x)` cannot be assigned to `ref x`
---
*/
struct S { uint x; }
void fail(ref uint);

void test21693(S s)
{
    fail(__rvalue(s.x));
    ref x = __rvalue(s.x);
}
