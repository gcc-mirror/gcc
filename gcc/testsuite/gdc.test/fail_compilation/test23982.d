/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test23982.d(35): Error: assigning scope variable `a` to non-scope parameter `a` calling `foo2` is not allowed in a `@safe` function
fail_compilation/test23982.d(26):        which is not `scope` because of `b = a`
---
*/
// https://issues.dlang.org/show_bug.cgi?id=23982
// Issue 23982 - segfault when printing scope inference failure
@safe:

struct B()
{
    this(int* a)
    {
        this.a = a;
    }
    int* a;
}

class C()
{
    int* foo2(int* a)
    {
        auto b = B!()(a);
     	return b.a;
    }
}

void main()
{
    scope int* a;
    C!() c;
    c.foo2(a);
}
