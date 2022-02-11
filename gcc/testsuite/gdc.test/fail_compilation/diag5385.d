/*
EXTRA_FILES: imports/fail5385.d
TEST_OUTPUT:
---
fail_compilation/diag5385.d(20): Error: no property `privX` for type `imports.fail5385.C`
fail_compilation/diag5385.d(21): Error: no property `packX` for type `imports.fail5385.C`
fail_compilation/diag5385.d(22): Error: no property `privX2` for type `imports.fail5385.C`
fail_compilation/diag5385.d(23): Error: no property `packX2` for type `imports.fail5385.C`
fail_compilation/diag5385.d(24): Error: no property `privX` for type `imports.fail5385.S`
fail_compilation/diag5385.d(25): Error: no property `packX` for type `imports.fail5385.S`
fail_compilation/diag5385.d(26): Error: no property `privX2` for type `imports.fail5385.S`
fail_compilation/diag5385.d(27): Error: no property `packX2` for type `imports.fail5385.S`
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
