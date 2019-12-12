/*
TEST_OUTPUT:
---
fail_compilation/ice11726.d(16): Error: undefined identifier `x`
---
*/

struct S
{
    auto opDispatch(string fn, Args...)(Args args)
    {
    }
}

void main() {
    S().reserve(x.foo());
}
