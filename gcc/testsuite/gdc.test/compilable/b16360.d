/*
REQUIRED_ARGS: -inline -wi

TEST_OUTPUT:
---
compilable/b16360.d(12): Warning: cannot inline function `b16360.foo`
compilable/b16360.d(25): Warning: cannot inline function `b16360.bar`
---
*/

pragma(inline, true)
auto foo()
{
    static struct U
    {
        int a = 42;
        float b;
        ~this(){} // __dtor: inline not allowed
    }
    U u;
    return u.a;
}

pragma(inline, true)
auto bar()
{
    class U   // class : inline not allowed
    {
        int a = 42;
        float b;
    }
    return (new U).a;
}

void main()
{
    auto f = foo();
    auto b = bar();
}
