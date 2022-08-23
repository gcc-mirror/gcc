import core.stdc.stdio;

enum real er1 = real.nan;
enum real er2 = 1;
static assert(er1 != er2);
static assert(!(er1 == er2));
static assert(!(er1 < er2));
static assert(!(er1 > er2));
static assert(!(er1 >= er2));
static assert(!(er1 <= er2));

enum double ed1 = real.nan;
enum double ed2 = 1;
static assert(ed1 != ed2);
static assert(!(ed1 == ed2));
static assert(!(ed1 < ed2));
static assert(!(ed1 > ed2));
static assert(!(ed1 >= ed2));
static assert(!(ed1 <= ed2));

bool b;


T byCTFE(T)()
{
    T x;
    return x;
}

bool bittst(const ubyte[] ba, uint pos)
{
    uint mask = 1 << (pos % 8);
    version(LittleEndian)
        return (ba[pos / 8] & mask) != 0;
    else
        return (ba[$ - 1 - pos / 8] & mask) != 0;
}

void test2(T)()
{
    T a = T.init, b = T.nan;
    assert(a is b);

    enum c = byCTFE!T();
    assert(a is c);

    static if (T.mant_dig == 64 && T.max_exp == 16384)
    {
        enum size = 10; // x87, exclude padding
        enum mant_dig = T.mant_dig;
    }
    else static if (T.mant_dig == 106)
    {
        enum size = 8; // IBM, only look at first index
        enum mant_dig = 53;
    }
    else
    {
        enum size = T.sizeof;
        enum mant_dig = T.mant_dig;
    }
    const pa = (cast(ubyte*) &a)[0 .. size];

    // the highest 2 bits of the mantissa should be set, everything else zero
    assert(bittst(pa, mant_dig - 1));
    assert(bittst(pa, mant_dig - 2));
    foreach(p; 0..mant_dig - 2)
        assert(!bittst(pa, p));
}

bool test()
{
        real r1 = real.nan;
        real r2 = 1;
        b = (r1 != r2); assert(b);
        b = (r1 == r2); assert(!b);
        b = (r1 <  r2); assert(!b);
        b = (r1 >  r2); assert(!b);
        b = (r1 <= r2); assert(!b);
        b = (r1 >= r2); assert(!b);

        double d1 = double.nan;
        double d2 = 1;
        b = (d1 != d2); assert(b);
        b = (d1 == d2); assert(!b);
        b = (d1 <  d2); assert(!b);
        b = (d1 >  d2); assert(!b);
        b = (d1 <= d2); assert(!b);
        b = (d1 >= d2); assert(!b);

        float f1 = float.nan;
        float f2 = 1;
        b = (f1 != f2); assert(b);
        b = (f1 == f2); assert(!b);
        b = (f1 <  f2); assert(!b);
        b = (f1 >  f2); assert(!b);
        b = (f1 <= f2); assert(!b);
        b = (f1 >= f2); assert(!b);
        return true;
}

void main()
{
    assert(test());
    test2!float();
    test2!double();
    test2!real();
}
