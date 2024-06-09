/*
TEST_OUTPUT:
---
fail_compilation/callconst.d(14): Error: function `func` is not callable using argument types `(const(X))`
fail_compilation/callconst.d(14):        cannot pass argument `x` of type `const(X)` to parameter `ref X`
fail_compilation/callconst.d(17):        `callconst.func(ref X)` declared here
---
*/
struct X {}

void main()
{
    auto x = const X();
    func(x);
}

void func(ref X);
