/*
EXTRA_FILES: imports/imp17602.d
TEST_OUTPUT:
---
fail_compilation/fail17602.d(17): Error: cannot implicitly convert expression `Status.on` of type `imports.imp17602.Status` to `fail17602.Status`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=17602

import imports.imp17602;

enum Status { off }

void main()
{
    Status status = imports.imp17602.Status.on;
}
