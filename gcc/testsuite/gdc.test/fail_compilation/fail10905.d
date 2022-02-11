/*
REQUIRED_ARGS: -m64
TEST_OUTPUT:
---
fail_compilation/fail10905.d(20): Error: incompatible types for `(this.x) == (cast(const(__vector(long[2])))cast(__vector(long[2]))1L)`: both operands are of type `const(__vector(long[2]))`
---
*/

struct Foo
{
    enum __vector(long[2]) y = 1;
}

struct Bar
{
    __vector(long[2]) x;

    bool spam() const
    {
        return x == Foo.y;
    }
}

