/+
TEST_OUTPUT:
---
fail_compilation/must_use_comma.d(16): Error: ignored value of `@mustuse` type `must_use_comma.S`; prepend a `cast(void)` if intentional
---
+/
import core.attribute;

@mustuse struct S {}

S fun() { return S(); }
void sideEffect() {}

void test()
{
    (fun(), sideEffect());
}
