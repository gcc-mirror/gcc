/*
TEST_OUTPUT:
---
fail_compilation/diag13528.d(6): Error: value of `this` is not known at compile time
fail_compilation/diag13528.d(6):        while evaluating `pragma(msg, __traits(getMember, A, "foo"))`
fail_compilation/diag13528.d(12):        parent scope from here: `mixin MyTemplate!()`
---
*/
#line 1

mixin template MyTemplate()
{
    void foo()
    {
        pragma(msg, __traits(getMember, typeof(this), "foo"));
    }
}

class A
{
    mixin MyTemplate;
}

void main()
{
    auto a = new A();
}
