/+
TEST_OUTPUT:
---
fail_compilation/must_use.d(17): Error: ignored value of `@mustuse` type `must_use.S`; prepend a `cast(void)` if intentional
fail_compilation/must_use.d(18): Error: ignored value of `@mustuse` type `must_use.S`; prepend a `cast(void)` if intentional
---
+/
import core.attribute;

@mustuse struct S {}

S fun();

void test()
{
    int x;
    fun();
    fun(), x++;
}
