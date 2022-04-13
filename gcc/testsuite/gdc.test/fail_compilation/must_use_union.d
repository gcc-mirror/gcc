/+
TEST_OUTPUT:
---
fail_compilation/must_use_union.d(15): Error: ignored value of `@mustuse` type `must_use_union.U`; prepend a `cast(void)` if intentional
---
+/
import core.attribute;

@mustuse union U {}

U fun() { return U(); }

void test()
{
    fun();
}
