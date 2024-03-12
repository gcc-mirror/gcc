// https://issues.dlang.org/show_bug.cgi?id=19948
// DISABLED: win32
/*
TEST_OUTPUT:
---
fail_compilation/fail19948.d(15): Error: function `fail19948.func(const(X))` is not callable using argument types `(X)`
fail_compilation/fail19948.d(15):        cannot pass argument `X()` of type `fail19948.main.X` to parameter `const(fail19948.X)`
---
*/
// DISABLED: win32
struct X {}
void main()
{
    struct X {}
    func(X());
}

void func(const(X)) {}
