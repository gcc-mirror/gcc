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
}
