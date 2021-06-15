// https://issues.dlang.org/show_bug.cgi?id=21939
/*
TEST_OUTPUT:
---
fail_compilation/test21939.d(9): Error: invalid `foreach` aggregate `Object`, define `opApply()`, range primitives, or use `.tupleof`
---
*/

static foreach (a; Object) {}
