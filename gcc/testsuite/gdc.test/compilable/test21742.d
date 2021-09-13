// https://issues.dlang.org/show_bug.cgi?id=21742

int foo()() { return 0; }

struct B
{
    int foo()() { return 0; }
}

static assert(is(typeof(foo) == void));

// failed, gagged error: expression B().foo()() has no type
static assert(is(typeof(B().foo) == void));
