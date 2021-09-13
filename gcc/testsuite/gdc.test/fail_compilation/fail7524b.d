// https://issues.dlang.org/show_bug.cgi?id=7524
/*
TEST_OUTPUT:
---
fail_compilation/fail7524b.d(10): Error: #line integer ["filespec"]\n expected
fail_compilation/fail7524b.d(10): Error: declaration expected, not `$n$L`
---
*/

#line 47 __VERSION__
