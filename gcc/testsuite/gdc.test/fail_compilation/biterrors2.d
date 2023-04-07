/* REQUIRED_ARGS: -preview=bitfields
 * TEST_OUTPUT:
---
fail_compilation/biterrors2.d(100): Error: variable `biterrors2.a` - bit-field must be member of struct, union, or class
fail_compilation/biterrors2.d(104): Error: bit-field `b` has zero width
fail_compilation/biterrors2.d(105): Error: bit-field type `float` is not an integer type
---
*/

#line 100
int a : 2;

struct S
{
    int b:0;
    float c:3;
}
