/* REQUIRED_ARGS: -preview=bitfields
 * TEST_OUTPUT:
---
fail_compilation/biterrors4.d(109): Error: cannot take address of bit-field `a`
---
*/

#line 100

struct S
{
    int a:3;
}

void test()
{
    S s;
    int* p = &s.a;
}
