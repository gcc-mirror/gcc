/+
TEST_OUTPUT:
---
fail_compilation/fail24301.d(18): Error: function `fail24301.fun(S __param_0)` is not callable using argument types `(S)`
fail_compilation/fail24301.d(18):        `ref S(ref S)` copy constructor cannot be used because it is annotated with `@disable`
---
+/
struct S
{
    @disable this(ref S);
}

@safe void fun(S) {}

@safe void main()
{
    S s;
    fun(s);
}
