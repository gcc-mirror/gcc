// https://issues.dlang.org/show_bug.cgi?id=24066

/*
TEST_OUTPUT:
---
false
---
*/

class C;
pragma(msg, __traits(isAbstractClass, C));
