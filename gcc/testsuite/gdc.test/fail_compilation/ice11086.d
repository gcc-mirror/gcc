/*
TEST_OUTPUT:
---
fail_compilation/ice11086.d(10): Error: template instance foo!A template 'foo' is not defined
---
*/

struct A
{
    foo!(A) l1,l2;
}
