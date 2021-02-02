/*
EXTRA_FILES: imports/imp21353.d
TEST_OUTPUT:
---
fail_compilation/test21353.d(19): Error: no property `A` for type `imports.imp21353.B`
fail_compilation/test21353.d(20): Error: no property `A` for type `imports.imp21353.B`
fail_compilation/test21353.d(21): Error: no property `A` for type `imports.imp21353.B`
fail_compilation/test21353.d(23): Error: undefined identifier `P` in module `imports.imp21353`
fail_compilation/test21353.d(24): Error: undefined identifier `P` in module `imports.imp21353`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=21353

import imports.imp21353;

void main()
{
    B.A;
    with (B) { A(0); }
    with (B()) { A(0); } // fixed

    imports.imp21353.P();
    with (imports.imp21353) { P(); } // fixed
}

