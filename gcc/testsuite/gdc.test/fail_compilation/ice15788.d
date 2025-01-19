/*
EXTRA_FILES: imports/range15788.d
TEST_OUTPUT:
---
fail_compilation/ice15788.d(20): Error: none of the overloads of `iota` are callable using argument types `!()(S, S)`
fail_compilation/imports/range15788.d(3):        Candidates are: `iota(B, E, S)(B, E, S)`
fail_compilation/ice15788.d(13):                        `ice15788.iota()`
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
