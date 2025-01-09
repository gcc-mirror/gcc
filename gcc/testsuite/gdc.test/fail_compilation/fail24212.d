/+
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fail24212.d(29): Error: assigning reference to local variable `n` to non-scope parameter `p` calling `fun` is not allowed in a `@safe` function
---
+/
class Base
{
    @safe pure nothrow
    void fun(int* p) {}
}

void test() @safe
{
    int* escaped;

    class Escaper : Base
    {
        @safe pure nothrow
        override void fun(int* p)
        {
            escaped = p;
        }
    }

    int n;
    Base base = new Escaper;
    base.fun(&n);
}
