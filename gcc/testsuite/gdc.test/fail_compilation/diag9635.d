// REQUIRED_ARGS: -m32
/*
TEST_OUTPUT:
---
fail_compilation/diag9635.d(17): Error: need 'this' for 'i' of type 'int'
fail_compilation/diag9635.d(18): Error: need 'this' for 'foo' of type 'pure nothrow @nogc @safe void()'
---
*/

struct Foo
{
    int i;
    void foo()() { }

    static void bar()
    {
        i = 4;
        foo();
    }
}
