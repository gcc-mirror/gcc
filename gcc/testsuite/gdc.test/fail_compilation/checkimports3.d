/*
REQUIRED_ARGS: -transition=checkimports -de
TEST_OUTPUT:
---
fail_compilation/checkimports3.d(14): Deprecation: local import search method found overloadset checkimports3.foo (3 overloads) instead of overloadset checkimports3.foo (2 overloads)
---
*/
import imports.checkimports3a;
import imports.checkimports3b;
import imports.checkimports3c;

void test()
{
    foo();
}
