// REQUIRED_ARGS: -de
// EXTRA_FILES: imports/imp15896.d
/*
TEST_OUTPUT:
---
fail_compilation/fail15896.d(11): Error: module `imports.imp15896` member `thebar` is not visible from module `fail15896`
fail_compilation/fail15896.d(11): Error: module `imports.imp15896` member `packagebar` is not visible from module `fail15896`
---
*/

import imports.imp15896 : thebar, packagebar;

int func()
{
    thebar +=1;
    packagebar += 1;
    return 0;
}
