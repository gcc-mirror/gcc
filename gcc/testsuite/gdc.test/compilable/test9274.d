// https://issues.dlang.org/show_bug.cgi?id=9274
struct S
{
    float[] arr;
    alias arr this;
}

static assert(!is(S == float[])); // ok
static assert(!is(S == T[], T)); // fails
static assert(is(S : float[]));
static assert(is(S : T[], T));

//https://issues.dlang.org/show_bug.cgi?id=9274
struct A(T)
{}

struct B
{
    A!int _a;
    alias _a this;
}

static assert(!is(B == A!int)); // OK
static assert(!is(B == A!X, X)); // assertion fails
static assert(is(B : A!int));
static assert(is(B : A!X, X));
