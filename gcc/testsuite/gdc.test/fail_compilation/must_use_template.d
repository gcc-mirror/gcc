/+
TEST_OUTPUT:
---
fail_compilation/must_use_template.d(15): Error: ignored value of `@mustuse` type `must_use_template.S!int`; prepend a `cast(void)` if intentional
---
+/
import core.attribute;

@mustuse struct S(T) {}

S!int fun() { return S!int(); }

void test()
{
    fun();
}
