alias AliasSeq(T...) = T;

struct Holder(T, ubyte val)
{
    T x = val;
}

struct SArrayHolder(T, ubyte val)
{
    T[2] x = val;
}

static foreach (T; AliasSeq!(bool, byte, short, int, long,
                             ubyte, ushort, uint, ulong,
                             char, wchar, dchar,
                             float, double, real))
{
    static assert(__traits(isZeroInit, T) == (T.init is T(0)));
    static assert(__traits(isZeroInit, T[2]) == (T.init is T(0)));

    static assert(!__traits(isZeroInit, Holder!(T, 1)));
    static assert(__traits(isZeroInit, Holder!(T, 0)));

    static assert(__traits(isZeroInit, SArrayHolder!(T, 0)));
    static assert(!__traits(isZeroInit, SArrayHolder!(T, 1)));

}

static assert(__traits(isZeroInit, void)); // For initializing arrays of element type `void`.
static assert(__traits(isZeroInit, void*));
static assert(__traits(isZeroInit, void[]));
static assert(__traits(isZeroInit, float[]));
static assert(__traits(isZeroInit, Object));
class C1 : Object
{
    int x = 1;
}
static assert(__traits(isZeroInit, C1)); // An Object's fields are irrelevant.

struct S1
{
    int[] a;
    int b;
}
static assert(__traits(isZeroInit, S1));

struct S2
{
    alias H = Holder!(int, 1);
    H h;
    int a;
}
static assert(!__traits(isZeroInit, S2));

struct S3
{
    S1 h;
    float f = 0;
}
static assert(__traits(isZeroInit, S3));

struct S4
{
    S2 h = S2(S2.H(0), 0);
    int a;
}
static assert(__traits(isZeroInit, S4));

struct S5
{
    Object o = null;
}
static assert(__traits(isZeroInit, S5));

template Vector(T) { alias __vector(T) Vector; }
static if (is(Vector!(int[4])))
{
    static assert(__traits(isZeroInit, Holder!(Vector!(int[4]), 0)));
    static assert(!__traits(isZeroInit, Holder!(Vector!(int[4]), 1)));
}

// https://issues.dlang.org/show_bug.cgi?id=24776
struct S6 {
    union {
        int i1;
        float f1;
    }
}
static assert(__traits(isZeroInit, S6));

struct S7
{
    union {
        float f2;
        int i2;
    }
}
static assert(!__traits(isZeroInit, S7));

// https://issues.dlang.org/show_bug.cgi?id=23841
union U
{
    float x = 0;
    float y;
}
static assert(__traits(isZeroInit, U));

union U2
{
    float x;
    int y;
}
static assert(!__traits(isZeroInit, U2));

struct S8 {
    int[0] dummy; // same offset as anon union, but doesn't overlap; should be ignored anyway for zero-init check
    union {
        float f; // is the first member of the anon union and must be checked
        int i;
    }
}
static assert(!__traits(isZeroInit, S8));
