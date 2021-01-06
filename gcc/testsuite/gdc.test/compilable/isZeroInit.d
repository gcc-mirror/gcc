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

version(D_SIMD):
import core.simd : int4;
static assert(__traits(isZeroInit, Holder!(int4, 0)));
static assert(!__traits(isZeroInit, Holder!(int4, 1)));
