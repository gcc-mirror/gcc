/*
TEST_OUTPUT:
---
fail_compilation/fail215.d(10): Error: function `fail215.b.k` cannot be both `final` and `abstract`
---
*/

class b
{
    final abstract void k();
}
