/*
EXTRA_FILES: imports/range15788.d
TEST_OUTPUT:
---
fail_compilation/ice15788.d(18): Error: none of the overloads of `iota` are callable using argument types `!()(S, S)`
---
*/

import imports.range15788 : iota;

void iota() {}

struct S {}

void main()
{
    S s;
    iota(s, s);
}
