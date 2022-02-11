// https://issues.dlang.org/show_bug.cgi?id=21246
// EXTRA_FILES: imports/test21246.d
/*
TEST_OUTPUT:
---
fail_compilation/test21246.d(16): Error: function `void test21246.C.set(Clock clock)` does not override any function, did you mean to override `void imports.test21246.B.set(imports.test21246.Clock clock)`?
---
*/
module test21246;

import imports.test21246;

class Clock { }
class C : B
{
    override void set (Clock clock) { }
}

void main () { }
