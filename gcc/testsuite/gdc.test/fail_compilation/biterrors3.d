/* REQUIRED_ARGS: -preview=bitfields
 * TEST_OUTPUT:
---
fail_compilation/biterrors3.d(103): Error: storage class not allowed for bit-field declaration
fail_compilation/biterrors3.d(106): Error: `d` is not a valid attribute for enum members
fail_compilation/biterrors3.d(106): Error: `:` is not a valid attribute for enum members
fail_compilation/biterrors3.d(106): Error: `3` is not a valid attribute for enum members
---
*/

#line 100

struct S
{
    static int : 3;
}

enum E { d : 3 }
