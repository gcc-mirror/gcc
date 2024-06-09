/*
EXTRA_FILES: imports/imp21353.d
TEST_OUTPUT:
---
fail_compilation/test21353.d(22): Error: no property `A` for type `imports.imp21353.B`
fail_compilation/imports/imp21353.d(5):        struct `B` defined here
fail_compilation/test21353.d(23): Error: no property `A` for type `imports.imp21353.B`
fail_compilation/imports/imp21353.d(5):        struct `B` defined here
fail_compilation/test21353.d(24): Error: no property `A` for type `imports.imp21353.B`
fail_compilation/imports/imp21353.d(5):        struct `B` defined here
fail_compilation/test21353.d(26): Error: undefined identifier `P` in module `imports.imp21353`
fail_compilation/test21353.d(27): Error: undefined identifier `P` in module `imports.imp21353`
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
