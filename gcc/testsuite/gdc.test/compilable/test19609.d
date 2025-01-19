// https://issues.dlang.org/show_bug.cgi?id=19609
// REQUIRED_ARGS: -verrors=simple
// EXTRA_FILES: imports/test19609a.d imports/test19609b.d imports/test19609c.d
/*
TEST_OUTPUT:
---
compilable/test19609.d(12): Deprecation: module `imports.test19609a` is deprecated
compilable/test19609.d(13): Deprecation: module `imports.test19609b` is deprecated - hello
compilable/test19609.d(14): Deprecation: module `imports.test19609c` is deprecated
---
*/
import imports.test19609a;
import imports.test19609b;
import imports.test19609c;
