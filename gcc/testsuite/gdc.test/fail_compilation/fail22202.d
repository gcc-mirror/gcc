// https://issues.dlang.org/show_bug.cgi?id=22202

/*
TEST_OUTPUT:
---
fail_compilation/fail22202.d(22): Error: function `fun` is not callable using argument types `(SystemCopy)`
fail_compilation/fail22202.d(22):        `inout ref inout(SystemCopy)(ref inout(SystemCopy) other)` copy constructor cannot be called from a `pure @safe nogc` context
fail_compilation/fail22202.d(17):        `fail22202.fun(SystemCopy __param_0)` declared here
---
*/

struct SystemCopy
{
    this(ref inout SystemCopy other) inout {}
}

void fun(SystemCopy) @safe pure @nogc {}

void main() @safe pure @nogc
{
    SystemCopy s;
    fun(s);
}
