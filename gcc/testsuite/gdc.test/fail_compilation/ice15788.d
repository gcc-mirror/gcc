/*
TEST_OUTPUT:
---
fail_compilation/ice15788.d(17): Error: none of the overloads of 'iota' are callable using argument types !()(S, S)
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
