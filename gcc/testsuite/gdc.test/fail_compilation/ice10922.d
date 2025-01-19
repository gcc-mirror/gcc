/*
TEST_OUTPUT:
---
fail_compilation/ice10922.d(11): Error: function `__lambda_L10_C12` is not callable using argument types `()`
fail_compilation/ice10922.d(11):        too few arguments, expected 1, got 0
fail_compilation/ice10922.d(10):        `ice10922.__lambda_L10_C12(in uint n)` declared here
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
