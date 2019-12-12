/*
TEST_OUTPUT:
---
fail_compilation/diag5385.d(27): Deprecation: imports.fail5385.C.privX is not visible from module diag5385
fail_compilation/diag5385.d(27): Error: class imports.fail5385.C member `privX` is not accessible
fail_compilation/diag5385.d(28): Deprecation: imports.fail5385.C.packX is not visible from module diag5385
fail_compilation/diag5385.d(28): Error: class imports.fail5385.C member `packX` is not accessible
fail_compilation/diag5385.d(29): Deprecation: imports.fail5385.C.privX2 is not visible from module diag5385
fail_compilation/diag5385.d(29): Error: class imports.fail5385.C member `privX2` is not accessible
fail_compilation/diag5385.d(30): Deprecation: imports.fail5385.C.packX2 is not visible from module diag5385
fail_compilation/diag5385.d(30): Error: class imports.fail5385.C member `packX2` is not accessible
fail_compilation/diag5385.d(31): Deprecation: imports.fail5385.S.privX is not visible from module diag5385
fail_compilation/diag5385.d(31): Error: struct imports.fail5385.S member `privX` is not accessible
fail_compilation/diag5385.d(32): Deprecation: imports.fail5385.S.packX is not visible from module diag5385
fail_compilation/diag5385.d(32): Error: struct imports.fail5385.S member `packX` is not accessible
fail_compilation/diag5385.d(33): Deprecation: imports.fail5385.S.privX2 is not visible from module diag5385
fail_compilation/diag5385.d(33): Error: struct imports.fail5385.S member `privX2` is not accessible
fail_compilation/diag5385.d(34): Deprecation: imports.fail5385.S.packX2 is not visible from module diag5385
fail_compilation/diag5385.d(34): Error: struct imports.fail5385.S member `packX2` is not accessible
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
