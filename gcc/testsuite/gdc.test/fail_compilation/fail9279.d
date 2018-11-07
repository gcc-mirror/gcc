/*
TEST_OUTPUT:
---
fail_compilation/fail9279.d(10): Error: escaping reference to stack allocated value returned by `b()`
fail_compilation/fail9279.d(13): Error: escaping reference to stack allocated value returned by `getArr()`
---
*/

char[2] b()() { char[2] ret; return ret; }
string a() { return b(); }

char[12] getArr() { return "Hello World!"; }
string getString() { return getArr(); }
