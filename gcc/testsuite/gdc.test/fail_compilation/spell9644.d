// REQUIRED_ARGS: -o-
// EXTRA_FILES: imports/spell9644a.d imports/spell9644b.d
/*
TEST_OUTPUT:
---
fail_compilation/spell9644.d(26): Error: undefined identifier `b`
fail_compilation/spell9644.d(27): Error: undefined identifier `xx`
fail_compilation/spell9644.d(28): Error: undefined identifier `cb`, did you mean variable `ab`?
fail_compilation/spell9644.d(29): Error: undefined identifier `bc`, did you mean variable `abc`?
fail_compilation/spell9644.d(30): Error: undefined identifier `ccc`
fail_compilation/spell9644.d(32): Error: undefined identifier `cor2`, did you mean variable `cor1`?
fail_compilation/spell9644.d(33): Error: undefined identifier `pua`, did you mean variable `pub`?
fail_compilation/spell9644.d(34): Error: undefined identifier `priw`
---
*/

import imports.spell9644a;

int a;
int ab;
int abc;
int cor1;

int main()
{
    cast(void)b; // max distance 0, no match
    cast(void)xx; // max distance 1, no match
    cast(void)cb; // max distance 1, match
    cast(void)bc; // max distance 1, match
    cast(void)ccc; // max distance 2, match

    cast(void)cor2; // max distance 1, match "cor1", but not cora from import (bug 13736)
    cast(void)pua;  // max distance 1, match "pub" from import
    cast(void)priw; // max distance 1, match "priv" from import, but do not report (bug 5839)
}
