// https://issues.dlang.org/show_bug.cgi?id=20427
extern(C++) void test20427(T)(T) {}
static assert(!__traits(compiles, { test20427([1, 2]); }));
