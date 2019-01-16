/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/test314.d(19): Deprecation: imports.a314.renamed is not visible from module test314
fail_compilation/test314.d(20): Deprecation: imports.a314.bug is not visible from module test314
fail_compilation/test314.d(22): Deprecation: imports.b314.renamedpkg is not visible from module test314
fail_compilation/test314.d(23): Deprecation: imports.b314.bugpkg is not visible from module test314
---
*/

module test314;

import imports.a314;
import imports.b314;

void main()
{
    renamed.bug("This should not work.\n");
    bug("This should not work.\n");

    renamedpkg.bug("This should not work.\n");
    bugpkg("This should not work.\n");
}
