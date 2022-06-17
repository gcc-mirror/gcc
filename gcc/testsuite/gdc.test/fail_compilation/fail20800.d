// https://issues.dlang.org/show_bug.cgi?id=20800

/*
TEST_OUTPUT:
---
fail_compilation/fail20800.d(22): Error: function `fail20800.fun(int a)` is not callable using argument types `(string)`
fail_compilation/fail20800.d(22):        cannot pass argument `(m()).index()` of type `string` to parameter `int a`
---
*/

struct RegexMatch
{
    string index() { return null; }
    ~this() { }
}
static m() { return RegexMatch(); }

void fun(int a);

void initCommands()
{
    fun(m.index);
}
