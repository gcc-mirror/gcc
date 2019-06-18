/*
TEST_OUTPUT:
---
fail_compilation/diag7747.d(8): Error: forward reference to inferred return type of function call `fact(n - 1)`
---
*/

auto fact(int n) { return n > 1 ? fact(n - 1) : 0; }

void main()
{
    fact(1);
}
