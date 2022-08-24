// REQUIRED_ARGS: -o- -preview=dip1000

void lazyfun(scope lazy int a) @nogc;

// Test that returning a local _static_ struct does not lead to allocation of a closure.
auto foo_static(int a, bool b) @nogc {
    static struct SInside {}

    SInside res;

    lazyfun(a);

    return res;
}

// Test that returning a local _non-static_ struct that does not reference any local variable does not lead to allocation of a closure.
auto foo_nonstatic(int a, bool b) @nogc {
    struct SInside {}

    SInside res;

    lazyfun(a);

    return res;
}

// Test that returning a local non-static struct that references a local variable does lead to allocation of a closure.
static assert(!__traits(compiles, () @nogc => goo(1)));
static assert(__traits(compiles, () => goo(1)));
auto goo(T)(T a) {
    struct SInside {
        T foo() { return a; }
    }

    SInside res;

    lazyfun(a);

    return res;
}
