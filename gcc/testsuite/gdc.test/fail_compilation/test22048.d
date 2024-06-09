// https://issues.dlang.org/show_bug.cgi?id=22048

/*
TEST_OUTPUT:
---
fail_compilation/test22048.d(10): Error: unexpected identifier `p` after `int`
---
*/

alias a = int p;
