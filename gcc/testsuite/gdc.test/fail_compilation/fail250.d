/*
TEST_OUTPUT:
---
fail_compilation/fail250.d(10): Error: constructor fail250.A.this default constructor for structs only allowed with @disable, no body, and no parameters
---
*/

struct A
{
    this() {}
}

void main()
{
    auto a = A();
}
