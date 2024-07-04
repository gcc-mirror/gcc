/+
TEST_OUTPUT:
---
fail_compilation/fail24301.d(19): Error: function `fun` is not callable using argument types `(S)`
fail_compilation/fail24301.d(19):        `ref S(ref S)` copy constructor cannot be used because it is annotated with `@disable`
fail_compilation/fail24301.d(14):        `fail24301.fun(S __param_0)` declared here
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
