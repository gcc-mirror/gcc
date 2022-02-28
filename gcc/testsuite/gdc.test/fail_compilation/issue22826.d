/* TEST_OUTPUT:
---
fail_compilation/issue22826.d(7): Error: #line integer ["filespec"]\n expected
fail_compilation/issue22826.d(7): Error: declaration expected, not `3`
---
*/
#line 12 "issue22826.d" 3
