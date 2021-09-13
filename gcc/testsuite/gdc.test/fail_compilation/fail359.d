/*
TEST_OUTPUT:
---
fail_compilation/fail359.d(8): Error: #line integer ["filespec"]\n expected
fail_compilation/fail359.d(9): Error: no identifier for declarator `_BOOM`
---
*/
#line 5 _BOOM
void main() { }

