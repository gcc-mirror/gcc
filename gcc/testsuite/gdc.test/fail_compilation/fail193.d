/*
TEST_OUTPUT:
---
fail_compilation/fail193.d(14): Error: cannot infer type from overloaded function symbol `& foo`
---
*/

void foo() { }
void foo(int) { }

void main()
{
    //void function(int) fp = &foo;
    auto fp = &foo;
    fp(1);
}
