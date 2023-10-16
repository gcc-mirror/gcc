/*
EXTRA_FILES: imports/fail5385.d
TEST_OUTPUT:
---
fail_compilation/diag5385.d(28): Error: no property `privX` for type `imports.fail5385.C`
fail_compilation/imports/fail5385.d(3):        class `C` defined here
fail_compilation/diag5385.d(29): Error: no property `packX` for type `imports.fail5385.C`
fail_compilation/imports/fail5385.d(3):        class `C` defined here
fail_compilation/diag5385.d(30): Error: no property `privX2` for type `imports.fail5385.C`
fail_compilation/imports/fail5385.d(3):        class `C` defined here
fail_compilation/diag5385.d(31): Error: no property `packX2` for type `imports.fail5385.C`
fail_compilation/imports/fail5385.d(3):        class `C` defined here
fail_compilation/diag5385.d(32): Error: no property `privX` for type `imports.fail5385.S`
fail_compilation/imports/fail5385.d(11):        struct `S` defined here
fail_compilation/diag5385.d(33): Error: no property `packX` for type `imports.fail5385.S`
fail_compilation/imports/fail5385.d(11):        struct `S` defined here
fail_compilation/diag5385.d(34): Error: no property `privX2` for type `imports.fail5385.S`
fail_compilation/imports/fail5385.d(11):        struct `S` defined here
fail_compilation/diag5385.d(35): Error: no property `packX2` for type `imports.fail5385.S`
fail_compilation/imports/fail5385.d(11):        struct `S` defined here
---
*/

import imports.fail5385;

void main()
{
    C.privX = 1;
    C.packX = 1;
    C.privX2 = 1;
    C.packX2 = 1;
    S.privX = 1;
    S.packX = 1;
    S.privX2 = 1;
    S.packX2 = 1;
}
