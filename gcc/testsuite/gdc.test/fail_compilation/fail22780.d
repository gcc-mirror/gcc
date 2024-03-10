// https://issues.dlang.org/show_bug.cgi?id=22780
/* TEST_OUTPUT:
---
fail_compilation/fail22780.d(12): Error: variable `fail22780.test10717.c` reference to `scope class` must be `scope`
---
*/

scope class C10717 { }

void test10717()
{
    C10717 c;
}
