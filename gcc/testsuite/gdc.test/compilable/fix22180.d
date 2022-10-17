// https://issues.dlang.org/show_bug.cgi?id=22180

align(8) { int x; }
//pragma(msg, x.alignof);
static assert(x.alignof == 8);
