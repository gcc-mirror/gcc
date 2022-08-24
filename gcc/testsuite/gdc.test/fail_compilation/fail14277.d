// https://issues.dlang.org/show_bug.cgi?id=14277

/*
TEST_OUTPUT:
---
fail_compilation/fail14277.d(10): Error: cannot implicitly convert expression `new char[](9999$?:32=u|64=LU$)` of type `char[]` to `ubyte[]`
---
*/

ubyte[] u = new char[9999];
