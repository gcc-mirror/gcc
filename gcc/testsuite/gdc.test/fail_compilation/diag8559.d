/*
TEST_OUTPUT:
---
fail_compilation/diag8559.d(12): Error: `void` does not have a default initializer
fail_compilation/diag8559.d(13): Error: `function` does not have a default initializer
---
*/

void foo(){}
void main()
{
    auto x = void.init;
    auto y = typeof(foo).init;
}
