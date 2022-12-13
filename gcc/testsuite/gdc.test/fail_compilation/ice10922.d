/*
TEST_OUTPUT:
---
fail_compilation/ice10922.d(10): Error: function `ice10922.__lambda4(in uint n)` is not callable using argument types `()`
fail_compilation/ice10922.d(10):        too few arguments, expected 1, got 0
---
*/

auto fib = (in uint n) pure nothrow {
    enum self = __traits(parent, {});
    return (n < 2) ? n : self(n - 1) + self(n - 2);
};

void main()
{
    auto n = fib(39);
}
