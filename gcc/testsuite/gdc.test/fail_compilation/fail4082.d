/*
TEST_OUTPUT:
---
fail_compilation/fail4082.d(14): Error: destructor `fail4082.Foo.~this` is not nothrow
fail_compilation/fail4082.d(12): Error: nothrow function `fail4082.test1` may throw
---
*/
struct Foo
{
    ~this() { throw new Exception(""); }
}
nothrow void test1()
{
    Foo f;

    goto NEXT;
NEXT:
    ;
}

/*
TEST_OUTPUT:
---
fail_compilation/fail4082.d(32): Error: destructor `fail4082.Bar.~this` is not nothrow
fail_compilation/fail4082.d(32): Error: nothrow function `fail4082.test2` may throw
---
*/
struct Bar
{
    ~this() { throw new Exception(""); }
}
nothrow void test2(Bar t)
{
}
