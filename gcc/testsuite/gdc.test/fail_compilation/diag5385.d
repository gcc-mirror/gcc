/*
TEST_OUTPUT:
---
fail_compilation/diag5385.d(19): Error: no property `privX` for type `imports.fail5385.C`, did you mean `imports.fail5385.C.privX`?
fail_compilation/diag5385.d(20): Error: no property `packX` for type `imports.fail5385.C`, did you mean `imports.fail5385.C.packX`?
fail_compilation/diag5385.d(21): Error: no property `privX2` for type `imports.fail5385.C`, did you mean `imports.fail5385.C.privX2`?
fail_compilation/diag5385.d(22): Error: no property `packX2` for type `imports.fail5385.C`, did you mean `imports.fail5385.C.packX2`?
fail_compilation/diag5385.d(23): Error: no property `privX` for type `S`, did you mean `imports.fail5385.S.privX`?
fail_compilation/diag5385.d(24): Error: no property `packX` for type `S`, did you mean `imports.fail5385.S.packX`?
fail_compilation/diag5385.d(25): Error: no property `privX2` for type `S`, did you mean `imports.fail5385.S.privX2`?
fail_compilation/diag5385.d(26): Error: no property `packX2` for type `S`, did you mean `imports.fail5385.S.packX2`?
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
