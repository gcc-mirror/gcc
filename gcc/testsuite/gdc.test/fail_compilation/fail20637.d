/*
EXTRA_FILES: imports/fail20637b.d
TEST_OUTPUT:
---
fail_compilation/fail20637.d(13): Error: no property `foo` for type `imports.fail20637b.A`
fail_compilation/imports/fail20637b.d(3):        class `A` defined here
---
*/
module fail20637;

import imports.fail20637b;

void main() { A.foo; }
