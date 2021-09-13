/*
REQUIRED_ARGS: -o-
TEST_OUTPUT:
----
fail_compilation/fail7524a.d(10): Error: #line integer ["filespec"]\n expected
fail_compilation/fail7524a.d(10): Error: declaration expected, not `"$r:\w+ +\d+ \d+$"`
----
*/

#line 47 __DATE__
