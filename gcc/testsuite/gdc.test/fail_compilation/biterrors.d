/* REQUIRED_ARGS: -preview=bitfields
 * TEST_OUTPUT:
---
fail_compilation/biterrors.d(103): Error: initializer not allowed for bit-field declaration
fail_compilation/biterrors.d(104): Error: storage class not allowed for bit-field declaration
---
 */

#line 100

struct S
{
    int i : 3 = 7;
    static int j : 3;
}
