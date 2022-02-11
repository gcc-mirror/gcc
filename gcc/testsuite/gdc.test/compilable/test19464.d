// https://issues.dlang.org/show_bug.cgi?id=19464

typeof(a0)    b0 = 3;
immutable int a0 = 4;
static assert(is(typeof(b0) == immutable(int)));
