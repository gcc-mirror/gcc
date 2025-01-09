/+
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/fail24208.d(19): Error: assigning reference to local variable `n` to non-scope parameter `p` calling `escape` is not allowed in a `@safe` function
fail_compilation/fail24208.d(15):        which is not `scope` because of `escaped = p`
---
+/
void test() @safe
{
    int* escaped;

    void escape(int* p) @safe
    {
        escaped = p;
    }

    int n;
    escape(&n);
}
