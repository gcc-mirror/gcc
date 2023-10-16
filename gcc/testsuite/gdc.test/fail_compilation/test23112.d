/* REQUIRED_ARGS: -betterC
TEST_OUTPUT:
---
fail_compilation/test23112.d(106): Error: function `test23112.bar` is `@nogc` yet allocates closure for `bar()` with the GC
fail_compilation/test23112.d(108):        function `test23112.bar.f` closes over variable `a`
fail_compilation/test23112.d(106):        `a` declared here
---
*/

// https://issues.dlang.org/show_bug.cgi?id=23112

#line 100

struct Forward(alias F)
{
    auto call()() { return F(); }
}

auto bar(int a) nothrow @safe
{
    auto f()
    {
        return a;
    }
    return Forward!f();
}

extern(C) void main()
{
    assert(bar(3).call() == 3);
}
