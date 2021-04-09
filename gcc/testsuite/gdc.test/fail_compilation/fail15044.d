/*
TEST_OUTPUT:
---
fail_compilation/fail15044.d(30): Error: generated function `fail15044.V.opAssign` cannot be used because it is annotated with `@disable`
---
*/

struct S
{
    void opAssign(S) {}
}

struct V
{
    // `s` has opAssign, so struct V needs to generate member-wise opAssign.
    // But S.opAssign is not callable on const object, so V.opAssign should be
    // @disable.
    const S s;

    // Here, the initializer of x is evaluated in V.semantic2. But
    // V.opAssign.semantic3 is not yet invoked, so its attribute should be
    // lazily inferred in functionSemantic even though it's non-instantiated function.
    enum int x = ()
    {
        // Here, the initializer of x is evaluated in V.semantic2, and
        // V.opAssign.semantic3 is not yet invoked in this time.
        // Therefore its @disable attribute needs to be inferred by
        // functionSemantic, even though it's non-instantiated function.
        V v;
        v = v;
    }();
}
