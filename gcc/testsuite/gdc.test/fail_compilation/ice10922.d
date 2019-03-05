/*
TEST_OUTPUT:
---
fail_compilation/ice10922.d(9): Error: function ice10922.__lambda4 (const(uint) n) is not callable using argument types ()
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
