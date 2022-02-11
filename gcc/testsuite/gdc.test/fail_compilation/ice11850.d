/*
EXTRA_FILES: imports/a11850.d
TEST_OUTPUT:
---
fail_compilation/ice11850.d(15): Error: incompatible types for `(a) < ([0])`: `uint[]` and `int[]`
fail_compilation/imports/a11850.d(9):        instantiated from here: `FilterResult!(__lambda1, uint[][])`
fail_compilation/ice11850.d(15):        instantiated from here: `filter!(uint[][])`
---
*/

import imports.a11850 : filter;

void main()
{
    filter!(a => a < [0])([[0u]]);
}
