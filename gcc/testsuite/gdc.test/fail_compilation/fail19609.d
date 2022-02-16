// https://issues.dlang.org/show_bug.cgi?id=19609
/*
EXTRA_FILES: imports/fail19609a.d imports/fail19609b.d imports/fail19609c.d imports/fail19609d.d
TEST_OUTPUT:
---
fail_compilation/imports/fail19609a.d(1): Error: `string` expected for deprecation message, not `([""])` of type `string[]`
fail_compilation/fail19609.d(16): Deprecation: module `imports.fail19609a` is deprecated
fail_compilation/imports/fail19609b.d(1): Error: `string` expected for deprecation message, not `([1])` of type `int[]`
fail_compilation/fail19609.d(17): Deprecation: module `imports.fail19609b` is deprecated
fail_compilation/imports/fail19609c.d(1): Error: `string` expected for deprecation message, not `(123.4F)` of type `float`
fail_compilation/fail19609.d(18): Deprecation: module `imports.fail19609c` is deprecated
fail_compilation/imports/fail19609d.d(1): Error: undefined identifier `msg`
fail_compilation/fail19609.d(20): Deprecation: module `imports.fail19609d` is deprecated
---
*/
import imports.fail19609a;
import imports.fail19609b;
import imports.fail19609c;
enum msg = "You should not be able to see me";
import imports.fail19609d;
