// https://issues.dlang.org/show_bug.cgi?id=19409

module test.foo;

static if (__traits(compiles,  __traits(identifier, test.foo))) {} // fails
else { static assert(0); }
