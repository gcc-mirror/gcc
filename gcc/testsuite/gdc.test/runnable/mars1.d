/*
REQUIRED_ARGS: -mcpu=native -preview=intpromote
PERMUTE_ARGS: -O -inline -release
*/

import core.stdc.stdio;

template tuple(A...) { alias tuple = A; }

///////////////////////

// https://github.com/dlang/dmd/pull/11441

long sdiv1(long l)
{
  return l / 2;
}

int sdiv2(int i)
{
  return i / 2;
}

void testsdiv2()
{
    assert(sdiv1(10) == 5);
    assert(sdiv1(-10) == -5);
    assert(sdiv2(10) == 5);
    assert(sdiv2(-10) == -5);
}

///////////////////////

void testulldiv()
{
    __gshared ulong[4][] vectors =
    [
        [10,3,3,1],
        [10,1,10,0],
        [3,10,0,3],
        [10,10,1,0],
        [10_000_000_000L, 11_000_000_000L, 0, 10_000_000_000L],
        [11_000_000_000L, 10_000_000_000L, 1, 1_000_000_000L],
        [11_000_000_000L, 11_000_000_000L, 1, 0],
        [10_000_000_000L, 10, 1_000_000_000L, 0],
        [0x8000_0000_0000_0000, 0x8000_0000_0000_0000, 1, 0],
        [0x8000_0000_0000_0001, 0x8000_0000_0000_0001, 1, 0],
        [0x8000_0001_0000_0000, 0x8000_0001_0000_0000, 1, 0],
        [0x8000_0001_0000_0000, 0x8000_0000_0000_0000, 1, 0x1_0000_0000],
        [0x8000_0001_0000_0000, 0x8000_0000_8000_0000, 1, 0x8000_0000],
        [0x8000_0000_0000_0000, 0x7FFF_FFFF_FFFF_FFFF, 1, 1],
        [0x8000_0000_0000_0000, 0x8000_0000_0000_0001, 0, 0x8000_0000_0000_0000],
        [0x8000_0000_0000_0000, 0x8000_0001_0000_0000, 0, 0x8000_0000_0000_0000],
    ];

    for (size_t i = 0; i < vectors.length; i++)
    {
        ulong q = vectors[i][0] / vectors[i][1];
        if (q != vectors[i][2])
            printf("[%zd] %lld / %lld = %lld, should be %lld\n",
                i, vectors[i][0], vectors[i][1], q, vectors[i][2]);

        ulong r = vectors[i][0] % vectors[i][1];
        if (r != vectors[i][3])
            printf("[%zd] %lld %% %lld = %lld, should be %lld\n",
                i, vectors[i][0], vectors[i][1], r, vectors[i][3]);
    }
}

////////////////////////////////////////////////////////////////////////

uint udiv10(uint x)
{
    return x / 10;
}

uint udiv14(uint x)
{
    return x / 14;
}

uint udiv14007(uint x)
{
    return x / 14007;
}

uint umod10(uint x)
{
    return x % 10;
}

uint umod14(uint x)
{
    return x % 14;
}

uint umod14007(uint x)
{
    return x % 14007;
}

uint uremquo10(uint x)
{
    return (x / 10) | (x % 10);
}

uint uremquo14(uint x)
{
    return (x / 14) | (x % 14);
}

uint uremquo14007(uint x)
{
    return (x / 14007) | (x % 14007);
}



ulong uldiv10(ulong x)
{
    return x / 10;
}

ulong uldiv14(ulong x)
{
    return x / 14;
}

ulong uldiv14007(ulong x)
{
    return x / 14007;
}

ulong ulmod10(ulong x)
{
    return x % 10;
}

ulong ulmod14(ulong x)
{
    return x % 14;
}

ulong ulmod14007(ulong x)
{
    return x % 14007;
}

ulong ulremquo10(ulong x)
{
    return (x / 10) | (x % 10);
}

ulong ulremquo14(ulong x)
{
    return (x / 14) | (x % 14);
}

ulong ulremquo14007(ulong x)
{
    return (x / 14007) | (x % 14007);
}


void testfastudiv()
{
  {
    static uint x10 = 10;
    static uint x14 = 14;
    static uint x14007 = 14007;

    uint u = 10000;
    uint r;
    r = udiv10(u);  assert(r == u/x10);
    r = udiv14(u);  assert(r == u/x14);
    r = udiv14007(u);  assert(r == u/x14007);
    r = umod10(u);  assert(r == u%x10);
    r = umod14(u);  assert(r == u%x14);
    r = umod14007(u);  assert(r == u%x14007);
    r = uremquo10(u);  assert(r == ((u/10)|(u%x10)));
    r = uremquo14(u);  assert(r == ((u/14)|(u%x14)));
    r = uremquo14007(u);  assert(r == ((u/14007)|(u%x14007)));
  }
  {
    static ulong y10 = 10;
    static ulong y14 = 14;
    static ulong y14007 = 14007;

    ulong u = 10000;
    ulong r;
    r = uldiv10(u);  assert(r == u/y10);
    r = uldiv14(u);  assert(r == u/y14);
    r = uldiv14007(u);  assert(r == u/y14007);
    r = ulmod10(u);  assert(r == u%y10);
    r = ulmod14(u);  assert(r == u%y14);
    r = ulmod14007(u);  assert(r == u%y14007);
    r = ulremquo10(u);  assert(r == ((u/10)|(u%y10)));
    r = ulremquo14(u);  assert(r == ((u/14)|(u%y14)));
    r = ulremquo14007(u);  assert(r == ((u/14007)|(u%y14007)));
  }
}


////////////////////////////////////////////////////////////////////////

// https://issues.dlang.org/show_bug.cgi?id=14936

long sldiv1 (long x) { return x / (1L << 1); }
long sldiv2 (long x) { return x / (1L << 2); }
long sldiv3 (long x) { return x / (1L << 3); }
long sldiv7 (long x) { return x / (1L << 7); }
long sldiv8 (long x) { return x / (1L << 8); }
long sldiv9 (long x) { return x / (1L << 9); }
long sldiv30(long x) { return x / (1L << 30); }
long sldiv31(long x) { return x / (1L << 31); }
long sldiv32(long x) { return x / (1L << 32); }
long sldiv33(long x) { return x / (1L << 33); }
long sldiv34(long x) { return x / (1L << 34); }
long sldiv62(long x) { return x / (1L << 62); }
long sldiv63(long x) { return x / (1L << 63); }

void testsldiv()
{
    /* Test special div code for signed long divide
     * by power of 2 for 32 bit targets.
     */

    // printf("63 = %llx\n", sldiv63(-0x7FFF_F8FF_FF3F_2FFFL));

    static foreach (C; tuple!(
                1,2,3,10,300,1000,
                4_1001_2030_0030,
                0x7FFF_F8FF_FF3F_2FFFL))
    {
        /* Check if runtime computation matches compile time
         */
        assert(sldiv1 ( C) ==  C / (1L << 1));
        assert(sldiv1 (-C) == -C / (1L << 1));
        assert(sldiv2 ( C) ==  C / (1L << 2));
        assert(sldiv2 (-C) == -C / (1L << 2));
        assert(sldiv3 ( C) ==  C / (1L << 3));
        assert(sldiv3 (-C) == -C / (1L << 3));
        assert(sldiv7 ( C) ==  C / (1L << 7));
        assert(sldiv7 (-C) == -C / (1L << 7));
        assert(sldiv8 ( C) ==  C / (1L << 8));
        assert(sldiv8 (-C) == -C / (1L << 8));
        assert(sldiv9 ( C) ==  C / (1L << 9));
        assert(sldiv9 (-C) == -C / (1L << 9));

        assert(sldiv30( C) ==  C / (1L << 30));
        assert(sldiv30(-C) == -C / (1L << 30));
        assert(sldiv31( C) ==  C / (1L << 31));
        assert(sldiv31(-C) == -C / (1L << 31));
        assert(sldiv32( C) ==  C / (1L << 32));
        assert(sldiv32(-C) == -C / (1L << 32));
        assert(sldiv33( C) ==  C / (1L << 33));
        assert(sldiv33(-C) == -C / (1L << 33));
        assert(sldiv34( C) ==  C / (1L << 34));
        assert(sldiv34(-C) == -C / (1L << 34));
        assert(sldiv62( C) ==  C / (1L << 62));
        assert(sldiv62(-C) == -C / (1L << 62));
        assert(sldiv63( C) ==  C / (1L << 63));
        assert(sldiv63(-C) == -C / (1L << 63));
    }
}

////////////////////////////////////////////////////////////////////////

// https://issues.dlang.org/show_bug.cgi?id=14936

long slmod1 (long x) { return x % (1L << 1); }
long slmod2 (long x) { return x % (1L << 2); }
long slmod3 (long x) { return x % (1L << 3); }
long slmod7 (long x) { return x % (1L << 7); }
long slmod8 (long x) { return x % (1L << 8); }
long slmod9 (long x) { return x % (1L << 9); }
long slmod30(long x) { return x % (1L << 30); }
long slmod31(long x) { return x % (1L << 31); }
long slmod32(long x) { return x % (1L << 32); }
long slmod33(long x) { return x % (1L << 33); }
long slmod34(long x) { return x % (1L << 34); }
long slmod62(long x) { return x % (1L << 62); }
long slmod63(long x) { return x % (1L << 63); }

void testslmod()
{
    static foreach (C; tuple!(
                1,2,3,10,300,1000,
                4_1001_2030_0030,
                0x7FFF_F8FF_FF3F_2FFFL))
    {
        /* Check if runtime computation matches compile time
         */
        assert(slmod1 ( C) ==  C % (1L << 1));
        assert(slmod1 (-C) == -C % (1L << 1));
        assert(slmod2 ( C) ==  C % (1L << 2));
        assert(slmod2 (-C) == -C % (1L << 2));
        assert(slmod3 ( C) ==  C % (1L << 3));
        assert(slmod3 (-C) == -C % (1L << 3));
        assert(slmod7 ( C) ==  C % (1L << 7));
        assert(slmod7 (-C) == -C % (1L << 7));
        assert(slmod8 ( C) ==  C % (1L << 8));
        assert(slmod8 (-C) == -C % (1L << 8));
        assert(slmod9 ( C) ==  C % (1L << 9));
        assert(slmod9 (-C) == -C % (1L << 9));

        assert(slmod30( C) ==  C % (1L << 30));
        assert(slmod30(-C) == -C % (1L << 30));
        assert(slmod31( C) ==  C % (1L << 31));
        assert(slmod31(-C) == -C % (1L << 31));
        assert(slmod32( C) ==  C % (1L << 32));
        assert(slmod32(-C) == -C % (1L << 32));
        assert(slmod33( C) ==  C % (1L << 33));
        assert(slmod33(-C) == -C % (1L << 33));
        assert(slmod34( C) ==  C % (1L << 34));
        assert(slmod34(-C) == -C % (1L << 34));
        assert(slmod62( C) ==  C % (1L << 62));
        assert(slmod62(-C) == -C % (1L << 62));
        assert(slmod63( C) ==  C % (1L << 63));
        assert(slmod63(-C) == -C % (1L << 63));
    }
}

////////////////////////////////////////////////////////////////////////

T divC(int C, T)(T x)
{
    T y = x;
    y /= C;
    assert(y == x / C);
    y = x;
    y /= -C;
    assert(y == x / -C);
    return x / C;
}

T modC(int C, T)(T x)
{
    T y = x;
    y %= C;
    assert(y == x % C);
    y = x;
    y %= -C;
    assert(y == x % -C);
    return x % C;
}

T remquoC(int C, T)(T x)
{
    return (x / C) | (x % C);
}

void testfastdiv()
{
    static int z = 0; // prevent constant folding by optimizer

    static foreach (T; tuple!(int, long, uint, ulong))
    {{
        T u = 10000;
        T r;
        static foreach (C; tuple!(10, 14, 14007, -10, -14, -14007))
        {
            r = divC!C(u);     assert(r == u / (z + C));
            r = modC!C(u);     assert(r == u % (z + C));
            r = remquoC!C(u);  assert(r == ((u / (z + C) | (u % (z + C)))));
        }
    }}
}

////////////////////////////////////////////////////////////////////////


/* Test the pattern:
 *   replace ((i / C1) / C2) with (i / (C1 * C2))
 * when e1 is 0 or 1 and (i2-i1) is a power of 2.
 */

void divdiv(T, T C1, T C2)(T i)
{
    auto a = (i / C1) / C2;
    auto b = i / (C1 * C2);
    if (a != b) assert(0);
}

void testdivdiv()
{
    divdiv!(int,10,20)(30);
    divdiv!(uint,10,20)(30);
    divdiv!(long,10,20)(30);
    divdiv!(ulong,10,20)(30);

    divdiv!(int,-10,20)(30);
    divdiv!(long,-10,20)(30);

    divdiv!(int,-10,-20)(-30);
    divdiv!(long,-10,-20)(-30);
}

////////////////////////////////////////////////////////////////////////

void testdivcmp()
{
    // https://github.com/dlang/dmd/pull/7128
    static bool foo(uint a, uint b)
    {
        return cast(bool)(a / b); // convert / to >=
    }

    assert(!foo(3, 4));
    assert(foo(4, 4));
    assert(foo(5, 4));
}

/////////////////////////////////////////////////////

void testgoto()
{
    int i;

    i = 3;
    goto L4;
L3: i++;
    goto L5;
L4: goto L3;
L5: assert(i == 4);
}

int testswitch()
{
    int i;

    i = 3;
    switch (i)
    {
        case 0:
        case 1:
        default:
            assert(0);
        case 3:
            break;
    }
    return 0;
}

void testdo()
{
    int x = 0;

    do
    {
        x++;
    } while (x < 10);
    printf("x == %d\n", x);
    assert(x == 10);
}


void testbreak()
{   int i, j;

  Louter:
    for (i = 0; i < 10; i++)
    {
        for (j = 0; j < 10; j++)
        {
            if (j == 3)
                break Louter;
        }
    }

    printf("i = %d, j = %d\n", i, j);
    assert(i == 0);
    assert(j == 3);
}

///////////////////////

int foo(string s)
{
    int i;

    i = 0;
    switch (s)
    {
        case "hello":
            i = 1;
            break;
        case "goodbye":
            i = 2;
            break;
        case "goodb":
            i = 3;
            break;
        default:
            i = 10;
            break;
    }
    return i;
}


void teststringswitch()
{   int i;

    i = foo("hello");
    printf("i = %d\n", i);
    assert(i == 1);

    i = foo("goodbye");
    printf("i = %d\n", i);
    assert(i == 2);

    i = foo("goodb");
    printf("i = %d\n", i);
    assert(i == 3);

    i = foo("huzzah");
    printf("i = %d\n", i);
    assert(i == 10);
}


///////////////////////

struct Foo
{
    int a;
    char b;
    long c;
}

Foo test(Foo f)
{
    f.a += 1;
    f.b += 3;
    f.c += 4;
    return f;
}


void teststrarg()
{
    Foo g;
    g.a = 1;
    g.b = 2;
    g.c = 3;

    Foo q;
    q = test(g);
    assert(q.a == 2);
    assert(q.b == 5);
    assert(q.c == 7);
}

///////////////////////

align (1) struct Foo1
{
  align (1):
    int a;
    char b;
    long c;
}

struct Foo2
{
    int a;
    char b;
    long c;
}

struct Foo3
{
    int a;
    align (1) char b;
    long c;
}

struct Foo4
{
    int a;
    struct { char b; }
    long c;
}

void testsizes()
{
    printf("%zd\n", Foo1.sizeof);
    assert(Foo1.a.offsetof == 0);
    assert(Foo1.b.offsetof == 4);
    assert(Foo1.c.offsetof == 5);
    assert(Foo1.sizeof == 13);

    assert(Foo2.a.offsetof == 0);
    assert(Foo2.b.offsetof == 4);
    assert(Foo2.c.offsetof == 8);
    assert(Foo2.sizeof == 16);

    assert(Foo3.a.offsetof == 0);
    assert(Foo3.b.offsetof == 4);
    assert(Foo3.c.offsetof == 8);
    assert(Foo3.b.sizeof == 1);
    assert(Foo3.sizeof == 16);

    assert(Foo4.sizeof == 16);
}

///////////////////////

size_t cond11565(size_t val)
{
    return val ? size_t.max : 0;
}

void test11565()
{
    assert(cond11565(true) == size_t.max);
}

///////////////////////

int[3] array1 = [1:1,2,0:3];

void testarrayinit()
{
    assert(array1[0] == 3);
    assert(array1[1] == 1);
    assert(array1[2] == 2);
}

///////////////////////

void test13023(ulong n)
{
    static void func(bool b) {}

    ulong k = 0;

    func(k >= n / 2);

    if (k >= n / 2)
        assert(0);
}

///////////////////////

struct U { int a; union { char c; int d; } long b; }

U f = { b:3, d:0x22222222, a:1 };

void testU()
{
    assert(f.b == 3);
    assert(f.d == 0x22222222);
    assert(f.c == 0x22);
    assert(f.a == 1);
    assert(f.sizeof == 16);
    assert(U.sizeof == 16);
}


////////////////////////////////////////////////////////////////////////

void vfunc() {}

void test12095(int k)
{
    int e = 0;
    e ? k || assert(0) : !e || vfunc();
    e ? k || assert(0) : e && vfunc();
    !e ? !e || vfunc() : k || assert(0);
}


////////////////////////////////////////////////////////////////////////


bool test3918a( float t, real u )
{
        printf("%Lf\n", u );
        return t && u;
}

bool test3918b( real t, float u )
{
        printf("%Lf\n", t );
        return t && u;
}

void test3918()
{
        assert(test3918a(float.nan, real.nan));
        assert(test3918b(real.nan, float.nan));
}

////////////////////////////////////////////////////////////////////////

T docond1(T)(T l, ubyte thresh, ubyte val) {
    l += (thresh < val);
    return l;
}

T docond2(T)(T l, ubyte thresh, ubyte val) {
    l -= (thresh >= val);
    return l;
}

T docond3(T)(T l, ubyte thresh, ubyte val) {
    l += (thresh >= val);
    return l;
}

T docond4(T)(T l, ubyte thresh, ubyte val) {
    l -= (thresh < val);
    return l;
}

void testdocond()
{
    assert(docond1!ubyte(10,3,5)  == 11);
    assert(docond1!ushort(10,3,5) == 11);
    assert(docond1!uint(10,3,5)   == 11);
    assert(docond1!ulong(10,3,5)  == 11);

    assert(docond2!ubyte(10,3,5)  == 10);
    assert(docond2!ushort(10,3,5) == 10);
    assert(docond2!uint(10,3,5)   == 10);
    assert(docond2!ulong(10,3,5)  == 10);

    assert(docond3!ubyte(10,3,5)  == 10);
    assert(docond3!ushort(10,3,5) == 10);
    assert(docond3!uint(10,3,5)   == 10);
    assert(docond3!ulong(10,3,5)  == 10);

    assert(docond4!ubyte(10,3,5)  == 9);
    assert(docond4!ushort(10,3,5) == 9);
    assert(docond4!uint(10,3,5)   == 9);
    assert(docond4!ulong(10,3,5)  == 9);


    assert(docond1!ubyte(10,5,3)  == 10);
    assert(docond1!ushort(10,5,3) == 10);
    assert(docond1!uint(10,5,3)   == 10);
    assert(docond1!ulong(10,5,3)  == 10);

    assert(docond2!ubyte(10,5,3)  == 9);
    assert(docond2!ushort(10,5,3) == 9);
    assert(docond2!uint(10,5,3)   == 9);
    assert(docond2!ulong(10,5,3)  == 9);

    assert(docond3!ubyte(10,5,3)  == 11);
    assert(docond3!ushort(10,5,3) == 11);
    assert(docond3!uint(10,5,3)   == 11);
    assert(docond3!ulong(10,5,3)  == 11);

    assert(docond4!ubyte(10,5,3)  == 10);
    assert(docond4!ushort(10,5,3) == 10);
    assert(docond4!uint(10,5,3)   == 10);
    assert(docond4!ulong(10,5,3)  == 10);
}

////////////////////////////////////////////////////////////////////////

struct S8658
{
    int[16385] a;
}

void foo8658(S8658 s)
{
    int x;
}

void test8658()
{
    S8658 s;
    for(int i = 0; i < 1000; i++)
        foo8658(s);
}

////////////////////////////////////////////////////////////////////////

uint neg(uint i)
{
    return ~i + 1;
}

uint com(uint i)
{
    return -i - 1;
}

float com(float i)
{
    return -i - 1;
}

uint com2(uint i)
{
    return -(i + 1);
}

void testnegcom()
{
    assert(neg(3) == -3);
    assert(com(3) == -4);
    assert(com(3.0f) == -4.0f);
    assert(com2(3) == -4);
}

////////////////////////////////////////////////////////////////////////

int oror1(char c)
{
    return ((((((((((cast(int) c <= 32 || cast(int) c == 46) || cast(int) c == 44)
                 || cast(int) c == 58) || cast(int) c == 59) || cast(int) c == 60)
              || cast(int) c == 62) || cast(int) c == 34) || cast(int) c == 92)
           || cast(int) c == 39) != 0);
}

int oror2(char c)
{
    return ((((((((((c <= 32 || c == 46) || c == 44)
                 || c == 58) || c == 59) || c == 60)
                 || c == 62) || c == 34) || c == 92)
                 || c == 39) != 0);
}

void testoror()
{
    assert(oror1(0) == 1);
    assert(oror1(32) == 1);
    assert(oror1(46) == 1);
    assert(oror1(44) == 1);
    assert(oror1(58) == 1);
    assert(oror1(59) == 1);
    assert(oror1(60) == 1);
    assert(oror1(62) == 1);
    assert(oror1(34) == 1);
    assert(oror1(92) == 1);
    assert(oror1(39) == 1);
    assert(oror1(33) == 0);
    assert(oror1(61) == 0);
    assert(oror1(93) == 0);
    assert(oror1(255) == 0);

    assert(oror2(0) == 1);
    assert(oror2(32) == 1);
    assert(oror2(46) == 1);
    assert(oror2(44) == 1);
    assert(oror2(58) == 1);
    assert(oror2(59) == 1);
    assert(oror2(60) == 1);
    assert(oror2(62) == 1);
    assert(oror2(34) == 1);
    assert(oror2(92) == 1);
    assert(oror2(39) == 1);
    assert(oror2(33) == 0);
    assert(oror2(61) == 0);
    assert(oror2(93) == 0);
    assert(oror2(255) == 0);
}

////////////////////////////////////////////////////////////////////////

bool bt1(int p, int a, int b)
{
    return p && ((1 << b) & a);
}

bool bt2(int p, long a, long b)
{
    return p && ((1L << b) & a);
}

void testbt()
{
    assert(bt1(1,7,2) == 1);
    assert(bt1(1,7,3) == 0);

    assert(bt2(1,0x7_0000_0000,2+32) == 1);
    assert(bt2(1,0x7_0000_0000,3+32) == 0);
}

////////////////////////////////////////////////////////////////////////

void test13383()
{
    foreach (k; 32..33)
    {
        if (1L & (1L << k))
        {
            assert(0);
        }
    }
}

////////////////////////////////////////////////////////////////////////

int andand1(int c)
{
    return (c > 32 && c != 46 && c != 44
                   && c != 58 && c != 59
                   && c != 60 && c != 62
                   && c != 34 && c != 92
                   && c != 39) != 0;
}

bool andand2(long c)
{
    return (c > 32 && c != 46 && c != 44
                   && c != 58 && c != 59
                   && c != 60 && c != 62
                   && c != 34 && c != 92
                   && c != 39) != 0;
}

int foox3() { return 1; }

int andand3(uint op)
{
    if (foox3() &&
        op != 7 &&
        op != 3 &&
        op != 18 &&
        op != 30 &&
        foox3())
        return 3;
    return 4;
}


void testandand()
{
    assert(andand1(0) == 0);
    assert(andand1(32) == 0);
    assert(andand1(46) == 0);
    assert(andand1(44) == 0);
    assert(andand1(58) == 0);
    assert(andand1(59) == 0);
    assert(andand1(60) == 0);
    assert(andand1(62) == 0);
    assert(andand1(34) == 0);
    assert(andand1(92) == 0);
    assert(andand1(39) == 0);
    assert(andand1(33) == 1);
    assert(andand1(61) == 1);
    assert(andand1(93) == 1);
    assert(andand1(255) == 1);

    assert(andand2(0) == false);
    assert(andand2(32) == false);
    assert(andand2(46) == false);
    assert(andand2(44) == false);
    assert(andand2(58) == false);
    assert(andand2(59) == false);
    assert(andand2(60) == false);
    assert(andand2(62) == false);
    assert(andand2(34) == false);
    assert(andand2(92) == false);
    assert(andand2(39) == false);
    assert(andand2(33) == true);
    assert(andand2(61) == true);
    assert(andand2(93) == true);
    assert(andand2(255) == true);

    assert(andand3(6) == 3);
    assert(andand3(30) == 4);
}

////////////////////////////////////////////////////////////////////////

bool bittest11508(char c)
{
    return c=='_' || c=='-' || c=='+' || c=='.';
}

void testbittest()
{
    assert(bittest11508('_'));
}

////////////////////////////////////////////////////////////////////////

uint or1(ubyte x)
{
    return x | (x<<8) | (x<<16) | (x<<24) | (x * 3);
}

void testor_combine()
{
    printf("%x\n", or1(1));
    assert(or1(5) == 5 * (0x1010101 | 3));
}

////////////////////////////////////////////////////////////////////////


int shrshl(int i) {
  return ((i+1)>>1)<<1;
}

void testshrshl()
{
    assert(shrshl(6) == 6);
    assert(shrshl(7) == 8);
}

////////////////////////////////////////////////////////////////////////

bool bt10715(in uint[] ary, size_t bitnum)
{
    return !!(ary[bitnum >> 5] & 1 << (bitnum & 31)); // uses bt
}

bool neg_bt10715(in uint[] ary, size_t bitnum)
{
    return !(ary[bitnum >> 5] & 1 << (bitnum & 31)); // does not use bt
}

void test10715()
{
    static uint[2]  a1 = [0x1001_1100, 0x0220_0012];

    if ( bt10715(a1,30)) assert(0);
    if (!bt10715(a1,8))  assert(0);
    if ( bt10715(a1,30+32)) assert(0);
    if (!bt10715(a1,1+32))  assert(0);

    if (!neg_bt10715(a1,30)) assert(0);
    if ( neg_bt10715(a1,8))  assert(0);
    if (!neg_bt10715(a1,30+32)) assert(0);
    if ( neg_bt10715(a1,1+32))  assert(0);
}

////////////////////////////////////////////////////////////////////////

ptrdiff_t compare12164(A12164* rhsPA, A12164* zis)
{
    if (*rhsPA == *zis)
        return 0;
    return ptrdiff_t.min;
}

struct A12164
{
    int a;
}

void test12164()
{
    auto a = A12164(3);
    auto b = A12164(2);
    assert(compare12164(&a, &b));
}

////////////////////////////////////////////////////////////////////////

int foo10678(char[5] txt)
{
    return txt[0] + txt[1] + txt[4];
}

void test10678()
{
    char[5] hello = void;
    hello[0] = 8;
    hello[1] = 9;
    hello[4] = 10;
    int i = foo10678(hello);
    assert(i == 27);
}

////////////////////////////////////////////////////////////////////////

struct S12051
{
    this(char c)
    {
        assert(c == 'P' || c == 'M');
    }
}

void test12051()
{
    auto ip = ["abc"];
    foreach (i, s; ip)
    {
        S12051(i < ip.length ? 'P' : 'M');
    }
}

////////////////////////////////////////////////////////////////////////

void bug7565( double x) { assert(x == 3); }

void test7565()
{
   double y = 3;
   bug7565( y++ );
   assert(y == 4);
}

////////////////////////////////////////////////////////////////////////

int bug8525(int[] devt)
{
    return devt[$ - 1];
}

////////////////////////////////////////////////////////////////////////

void func13190(int) {}

struct Struct13190
{
    ulong a;
    uint b;
};

__gshared Struct13190* table13190 =
[
    Struct13190(1, 1),
    Struct13190(0, 2)
];

void test13190()
{
    for (int i = 0; table13190[i].a; i++)
    {
        ulong tbl = table13190[i].a;
        func13190(i);
        if (1 + tbl)
        {
            if (tbl == 0x80000)
                return;
        }
    }
}

////////////////////////////////////////////////////////////////////////

double foo13485(double c, double d)
{
    // This must not be optimized to c += (d + d)
    c += d;
    c += d;
    return c;
}

void test13485()
{
    enum double d = 0X1P+1023;
    assert(foo13485(-d, d) == d);
}

////////////////////////////////////////////////////////////////////////

void test12833a(int a)
{
    long x = cast(long)a;

    switch (cast(int)(cast(ushort)(x >> 16 & 65535L)))
    {
        case 1:
        {
            break;
        }
        default:
        {
            assert(0);
        }
    }
}

void test12833()
{
    test12833a(0x1_0000);
}

/***********************************************/

struct Point9449
{
    double f = 3.0;
    double g = 4.0;
}

void test9449()
{
    Point9449[1] arr;
    if (arr[0].f != 3.0) assert(0);
    if (arr[0].g != 4.0) assert(0);
}

struct Point9449x
{
    float  f = 0.0;
    double g = 0.0;
}

void test9449x()
{
    Point9449x[1] arr;
    if (arr[0].f != 0.0) assert(0);
    if (arr[0].g != 0.0) assert(0);
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=12057

bool prop12057(real x) { return false; }
double f12057(real) { return double.init; }
void test12057()
{
    real fc = f12057(real.init);
    if (fc == 0 || fc.prop12057) {}
}


////////////////////////////////////////////////////////////////////////

long modulo24 (long ticks)
{
    ticks %= 864000000000;
    if (ticks < 0)
        ticks += 864000000000;
    return ticks;
}

void test13784()
{
    assert (modulo24(-141600000000) == 722400000000);
}


////////////////////////////////////////////////////////////////////////

struct S13969 {
    int x, y;
}

int test13969(const S13969* f) {
    return 0 % ((f.y > 0) ? f.x / f.y : f.x / -f.y);
}

////////////////////////////////////////////////////////////////////////

int[] arr14436;
void test14436()
{
    assert(arr14436 == null);
    arr14436 = [1, 2, 3];
    assert(arr14436 != null);
}

////////////////////////////////////////////////////////////////////////

void test14220()
{
    auto a = toString(14);

    printf("a.ptr = %p, a.length = %d\n", a.ptr, cast(int)a.length);
    return;
}

auto toString(int value)
{
    uint mValue = value;

    char[int.sizeof * 3] buffer = void;
    size_t index = buffer.length;

    do
    {
        uint div = cast(int)(mValue / 10);
        char mod = mValue % 10 + '0';
        buffer[--index] = mod;        // Line 22
        mValue = div;
    } while (mValue);

    //printf("buffer.ptr = %p, index = %d\n", buffer.ptr, cast(int)index);
    return dup(buffer[index .. $]);
}

char[] dup(char[] a)
{
    //printf("a.ptr = %p, a.length = %d\n", a.ptr, cast(int)a.length);
    a[0] = 1;       // segfault
    return a;
}

////////////////////////////////////////////////////////////////////////

int stripLeft(int str, int dc)
{
    while (true)
    {
        int a = str;
        int s = a;
        str += 1;
        if (dc) return s;
    }
}

void test14829()
{
    if (stripLeft(3, 1) != 3) // fails with -O
        assert(0);
}


////////////////////////////////////////////////////////////////////////

void test3()
{
    int[6] a;
    int[] b;
    b = a;
    b = (b.ptr + b.length - 5)[0 .. b.ptr + b.length - 1 - a.ptr];
    assert(b.ptr == a.ptr + 1);
    assert(b.length == 5);
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=14782


void test14782()
{
    static struct Foo
    {
        long a = 8;
        int b = 7;
    }

    static Foo[1] fun() { Foo[1] a; return a; }

    auto result = fun();
    assert(result[0].a == 8);
    assert(result[0].b == 7);
}

////////////////////////////////////////////////////////////////////////

void test14987()
{
    static struct Foo
    {
        int b = 7;
    }
    static assert((Foo[4]).sizeof == 16);

    static Foo[4] fun() { Foo[4] a; return a; }

    auto result = fun();
    assert(result[0].b == 7);
    assert(result[1].b == 7);
    assert(result[2].b == 7);
    assert(result[3].b == 7);
}

////////////////////////////////////////////////////////////////////////

void[] calloc15272(size_t bc) nothrow pure
{
    assert(bc == 1);
    return new void[1];
}

void test15272()
{
    void[] scache = cast(void[])"abc";
    size_t count = 1;
    void[]* buckets = &scache;
    *buckets = calloc15272(count)[0 .. count];
}

/*****************************************
 * https://issues.dlang.org/show_bug.cgi?id=15861
 */

void test15861()
{
    double val = 4286853117.;

    (){
        assert(val == 4286853117.);
    }();
}

////////////////////////////////////////////////////////////////////////

// https://issues.dlang.org/show_bug.cgi?id=15629 comment 3
// -O

void test15629()
{
    int[] a = [3];
    int value = a[0] >= 0 ? a[0] : -a[0];
    assert(a[0] == 3);
    writeln(value, a);
}

void writeln(int v, int[] a)
{
}

////////////////////////////////////////////////////////////////////////

real binPosPow2() { return 1.0L; }

real binPow2()
{
    return 1.0L/binPosPow2();
}

void test4()
{
    assert(binPow2() == 1.0L);
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=13474


double sumKBN(double s = 0.0)
{
    import core.math : fabs;
    double c = 0.0;
        foreach(double x; [1, 1e100, 1, -1e100])
        {
            x = multiply(x);
            double t = s + x;
            if(s.fabs >= x.fabs)
            {
                double y = s-t;
                c += y+x;
            }
            else
            {
                double y = x-t;
                c += y+s;
            }
            s = t;
        }
    return s + c;
}

double multiply(double a) { return a * 10000; }

void test13474()
{
    double r = 20000;
    assert(r == sumKBN());
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=16699

ulong[1] parseDateRange()
{
    try
    {
        ulong[1] result;
        result[0] = 6;
        return result;
    }
    finally
    {
    }
}

void test16699()
{
    ulong[1] range = parseDateRange();
    assert(range[0] == 6);
}

////////////////////////////////////////////////////////////////////////

// https://issues.dlang.org/show_bug.cgi?id=16102

struct S16102 { ~this() { } }

long[1] f16102()
{
    S16102 a;
    return [1];
}

void test16102()
{
    assert( f16102() == [1] );
}

////////////////////////////////////////////////////////////////////////

void test5a(ulong x, ulong y)
{
    int a;
    if (x >> 32)
        a = 1;
    else
        a = 2;
    assert(a == 1);

    if (y >> 32)
        a = 1;
    else
        a = 2;
    assert(a == 2);
}

void test5()
{
    test5a(uint.max + 1L, uint.max);
}

////////////////////////////////////////////////////////////////////////

/* Test the pattern:
 *   replace (e ? i1 : i2) with (i1 + e * (i2 - i1))
 * when e1 is 0 or 1 and (i2-i1) is a power of 2.
 */

int foo61(int i)
{
    return (i % 2 != 0) ? 4 : 2;
}

int foo62(int i)
{
    return (i % 2 != 0) ? 2 : 4;
}

bool bar6(bool b) { return b; }

int foo63(bool b)
{
    return bar6(b) ? 16 : 8;
}

int foo64(bool b)
{
    return bar6(b) ? 8 : 16;
}

void test6()
{
    if (foo61(0) != 2) assert(0);
    if (foo61(1) != 4) assert(0);
    if (foo62(0) != 4) assert(0);
    if (foo62(1) != 2) assert(0);
    if (foo63(0) != 8) assert(0);
    if (foo63(1) != 16) assert(0);
    if (foo64(0) != 16) assert(0);
    if (foo64(1) != 8) assert(0);
}

////////////////////////////////////////////////////////////////////////

int dataflow(int b) {
  int ret;

  if (b==4)
    ret = 3;
  else
    ret = 5;

  if (ret == 4)
    return 0;
  else
    return 1;
}

void testeqeqranges()
{
    int i = dataflow(4);
    if (i != 1)
        assert(0);
}

////////////////////////////////////////////////////////////////////////

// https://issues.dlang.org/show_bug.cgi?id=16189

void test16189()
{
    ubyte[9][1] data;
    uint a = 0;
  loop:
    data[0] = data[a];
    a--;
    bool b = false;
    if (b) goto loop;
    assert(a == -1); // was failing with -O
}


////////////////////////////////////////////////////////////////////////

// https://issues.dlang.org/show_bug.cgi?id=16997

void test16997()
{
    /* Exhaustively test all signed and unsigned byte promotions for
     * - + and ~
     */
    for (int i = 0; i < 256; ++i)
    {
        ubyte c = cast(ubyte)i;

        int i1 = cast(int)(~c);
        int i2 = cast(int)(~cast(int)c);

        //printf("%d, %d\n", i1, i2);
        assert(i1 == i2);

        i1 = cast(int)(+c);
        i2 = cast(int)(+cast(int)c);
        assert(i1 == i2);

        i1 = cast(int)(-c);
        i2 = cast(int)(-cast(int)c);
        assert(i1 == i2);
    }

    for (int i = 0; i < 256; ++i)
    {
        byte c = cast(byte)i;

        int i1 = cast(int)(~c);
        int i2 = cast(int)(~cast(int)c);

        //printf("%d, %d\n", i1, i2);
        assert(i1 == i2);

        i1 = cast(int)(+c);
        i2 = cast(int)(+cast(int)c);
        assert(i1 == i2);

        i1 = cast(int)(-c);
        i2 = cast(int)(-cast(int)c);
        assert(i1 == i2);
    }
}

////////////////////////////////////////////////////////////////////////

void test18315() // https://issues.dlang.org/show_bug.cgi?id=18315
{
    int i = int.min;
    bool b = i > 0;
    assert(!b);
    b = 0 < i;
    assert(!b);
}

////////////////////////////////////////////////////////////////////////

// https://issues.dlang.org/show_bug.cgi?id=18461

void test18461()
{
    import core.bitop;

    size_t test_val = 0b0001_0000;

    if (bt(&test_val, 4) == 0)
        assert(false);
}

////////////////////////////////////////////////////////////////////////

void test18730() // https://issues.dlang.org/show_bug.cgi?id=18730
{
    static if (size_t.sizeof == 8)
    {
        static int bt18730_64_64(in ulong* p, ulong bitnum) pure @system
        {
            return ((p[bitnum >> 6] & (1L << (bitnum & 63)))) != 0;
        }
        static int bt18730_64_32(in ulong* p, uint bitnum) pure @system
        {
            return ((p[bitnum >> 6] & (1L << (bitnum & 63)))) != 0;
        }
        static int bt18730_32_64(in uint* p, ulong bitnum) pure @system
        {
            return ((p[bitnum >> 5] & (1 << (bitnum & 31)))) != 0;
        }

        // Check that bt_64_64 uses a 64-bit register for the offset.
        {
            enum bitIndex = int.max + 1L;
            auto a = new ulong[](bitIndex / 64 + 1);
            a[bitIndex / 64] = 1;
            assert(bt18730_64_64(a.ptr, bitIndex));
            assert(!bt18730_64_64(a.ptr, bitIndex + 1));
            assert(!bt18730_64_64(a.ptr, bitIndex - 1));
        }
        // Check that bt_64_32 uses a 32-bit register for the offset.
        {
            static int f(ulong* p, ulong bitnum)
            {
                return bt18730_64_32(p, cast(uint) bitnum);
            }
            enum bitIndex = uint.max + 1L;
            assert(cast(uint) bitIndex == 0);
            ulong s = 1;
            assert(f(&s, bitIndex));
        }
        /* Check that bt_32_64 does not become a 64-bit bt instruction. Would lead
        to a segfault when trying to load 8 bytes while only 4 are accessible. */
        version (Posix)
        {{
            import core.sys.posix.sys.mman;
            import core.sys.posix.unistd;
            // Allocate two pages.
            immutable sz = 2 * sysconf(_SC_PAGESIZE);
            auto m = mmap(null, sz, PROT_READ, MAP_PRIVATE | MAP_ANON, -1, 0);
            // Discard the higher page. It becomes unreadable.
            munmap(m + sz / 2, sz / 2);
            // Try looking at the last 4 bytes of the readable page.
            uint* p = cast(uint*) (m + sz / 2 - uint.sizeof);
            bt18730_32_64(p, 0);
            munmap(m, sz / 2); // Free the readable page.
        }}
    }
}

////////////////////////////////////////////////////////////////////////

void test19497() // https://issues.dlang.org/show_bug.cgi?id=19497
{
    {
        ubyte[1024] data;
        ushort* ushortPtr = cast(ushort*) data.ptr;
        *ushortPtr++ = 0xfe00;
        printf("ushortPtr(%p)\n", ushortPtr);
        fflush(stdout);
    }

    alias Seq(stuff ...) = stuff;
    static foreach (T; Seq!(ubyte, ushort, uint, ulong, byte, short, int, long))
    {{
        T[2] data = 0x2A;
        T* q = &data[0];
        *q++ = cast(T) 0x1122334455667788;
        if (*q != 0x2A) assert(false);
    }}

    {
        static int toStringz(string s) { return s.length > 0 ? s[0] : 0; }
        static void toAStringz(in string[] a, int* az)
        {
            foreach (string s; a)
            {
                *az++ = toStringz(s);
            }
        }
        string[1] sa = ["abc"];
        int[2] tgt = 0x2a;
        toAStringz(sa[], tgt.ptr);
        if (tgt[0] != 'a') assert(false);
        if (tgt[1] != 0x2a) assert(false);
    }
}

////////////////////////////////////////////////////////////////////////

// https://issues.dlang.org/show_bug.cgi?id=18794

bool method18794(size_t* p)
{
    int bitIdx = 0;
    func18794();
    return (*p & (1UL << bitIdx)) != 0;
}

void func18794() {}

void prep18794()
{
    asm {}
    ulong[2] x = -1;
}

void test18794()
{
    prep18794();
    size_t s;
    method18794(&s);
}

////////////////////////////////////////////////////////////////////////

/* Test the optimization
 *  (e1+c)-e2 => (e1-e2)+c
 */

void testelmin()
{
    static void foo(int i)
    {
        static ubyte[4] bar()
        {
            ubyte[4] array;
            foreach (i, ref a; array)
                a = cast(ubyte)(i + 1);
            return array;
        }

        static void test(int i, ubyte* p)
        {
            foreach (j; 0 .. 4)
                assert(p[i * 4 + j] == j + 1);
        }

        ubyte[32] data;
        data[i*4..(i+1)*4] = bar(); // optimize to single MOV

        test(i, data.ptr);
    }

    foo(4);
}

////////////////////////////////////////////////////////////////////////

const(char)* fastpar(string s)
{
    return s.ptr + s.length;
}

void testfastpar()
{
    string s = "abcde";
    auto p = fastpar(s);
    assert(*p == 0);
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=20363

ulong foo20363(double d)
{
    ulong u = * cast(ulong*) &d;
    return (u >> 1) & 1;
}

void test20363()
{
    ulong u = 0b10;
    if (foo20363(*cast(double*) &u) == 0)
        assert(false);
}

////////////////////////////////////////////////////////////////////////


T testfooa(T)(T value)
{
    return 10 - (value * 57); // gets rewritten into (value*-57)+10
}

T testfoob(T)(T value)
{
    return (value * -57) + 10;
}

void testNegConst()
{
    assert(testfooa(1) == -47);
    assert(testfoob(1) == -47);
    assert(testfooa(1.0) == -47);
    assert(testfoob(1.0) == -47);
}

////////////////////////////////////////////////////////////////////////

// https://issues.dlang.org/show_bug.cgi?id=16317

int add8ret3(ref int s)
{
    s += 8;
    return 3;
}

int binAdd(int val)
{
    val = val + add8ret3(val);
    return val;
}

void test16317()
{
    assert(binAdd(1) == (1 + 3));
    static assert(binAdd(1) == (1 + 3));
}

////////////////////////////////////////////////////////////////////////

// https://issues.dlang.org/show_bug.cgi?id=20050

int test20050_g = 0;
void test20050_impure_function_1() { ++test20050_g; }
void function() test20050_get_impure_function() pure
{
    static void impure_function_2()
    {
        ++test20050_g;
        test20050_impure_function_1();
    }
    return &impure_function_2;
}
void test20050()
{
    auto f = test20050_get_impure_function();
    f();
    assert(test20050_g == 2);
}

////////////////////////////////////////////////////////////////////////

// http://github.com/dlang/dmd/pull/11238

int testCpStatic1(int y)
{
    __gshared int yyy = 7;
    auto x = yyy; // no copy-propagation
    if (y)
        return x;
    return x + 3;
}

void testCpStatic()
{
    assert(testCpStatic1(1) == 7);
    assert(testCpStatic1(0) == 10);
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=20991

int x7;

void bar7(int i)
{
    assert(i == x7);
    ++x7;
}

void test7()
{
    for (int i = 0; i <= 1; ++i)
        bar7(i);
    assert(x7 == 2);
}

////////////////////////////////////////////////////////////////////////

// http://github.com/dlang/dmd/pull/11388

ushort byteswap(ushort x) pure
{
    // Should be detected and XCHG instruction generated
    return cast(ushort) (((x >> 8) & 0xFF) | ((x << 8) & 0xFF00u));
}

void testbyteswap()
{
    assert(byteswap(0xF234) == 0x34F2);
    static ushort xx = 0xF234;
    assert(byteswap(xx) == 0x34F2);
}

////////////////////////////////////////////////////////////////////////

// These should all be recognized by the compiler and generate ROL or ROR
// instructions.

uint rol32(uint x, uint n)
{
    return (x << n) | (x >> (32 - n));
}

uint ror32(uint x, uint n)
{
    return (x >> n) | (x << (32 - n));
}

ulong rol64(ulong x, uint n)
{
    return (x << n) | (x >> (64 - n));
}

ulong ror64(ulong x, uint n)
{
    return (x >> n) | (x << (64 - n));
}

void testrolror()
{
    assert(ror32(0x0123_4567u, 4) == 0x7012_3456);
    assert(rol32(0x7012_3456u, 4) == 0x0123_4567);

    assert(ror64(0x0123_4567_89AB_CDEFuL, 4) == 0xF012_3456_789A_BCDE);
    assert(rol64(0xF012_3456_789A_BCDEuL, 4) == 0x0123_4567_89AB_CDEF);
}

////////////////////////////////////////////////////////////////////////

// https://issues.dlang.org/show_bug.cgi?id=20162

void test20162()
{
    static long f(long a)
    {
         assert(a == -1L);
         return a;
    }

    foreach (i; 1 .. 2)
    {
        foreach (j; 0 .. 2)
        {
            printf("%d %d %llx\n", i,
              ((i != 0) ? -1 : +1),
              f((i != 0) ? -1 : +1));
        }
    }
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=3713

int star1(int i)
{
    return i ? star1(i - 1) : 0;
}

int star2(int i)
{
    return i == 0 ? 0 : star2(i - 1);
}

int star3(int i)
{
    if (i == 0)
        return 0;
    return i == 2 ? star3(i - 2) : star3(i - 1);
}

int star4(int i)
{
    return (i == 0) ? 0
          : i != 2  ? star4(i - 1)
          : star4(i - 2);
}

void test3713()
{
    assert(star1(10) == 0);
    assert(star2(10) == 0);
    assert(star3(10) == 0);
    assert(star4(10) == 0);
}

////////////////////////////////////////////////////////////////////////

void testsbbrex()
{
    // special code is generated for these two cases
    static long foolt(dchar c)
    {
        return c < 0x10000 ? 1 : 2;
    }

    static long fooge(uint c)
    {
        return c >= 0x10000 ? 1L : 2L;
    }

    assert(foolt(0) == 1);
    assert(foolt(0x10000) == 2);
    assert(fooge(0) == 2);
    assert(fooge(0x10000) == 1);
}


////////////////////////////////////////////////////////////////////////

// https://issues.dlang.org/show_bug.cgi?id=19846

alias Void = byte[0];
static immutable Void VOID; // = [];

__gshared int x19846;

Void print19846()
{
    //printf("This should print\n");
    x19846 = 3;
    return VOID;
}

Void identity19846(Void value, out int i)
{
    i = 7;
    return value;
}

void test19846()
{
    int i;
    identity19846(print19846(), i);
    //printf("i = %d\n", i);
    assert(x19846 == 3);
    assert(i == 7);
}

////////////////////////////////////////////////////////////////////////

// Some tests for OPmemcpy

enum N = 128;

ubyte[N] def()
{
    ubyte[N] array;
    foreach (i, ref a; array)
        a = cast(ubyte)(i + 1);
    return array;
}


void ghi(ubyte* p)
{
    foreach (i; 0 .. N)
        assert(p[i] == i + 1);
}

void testmemcpy()
{
    ubyte[N] bits;
    ubyte[N] bits2;
    bits2[0..N] = bits[0..N] = def();
    ghi(bits.ptr);
    ghi(bits2.ptr);

    __gshared size_t n = N;
    ubyte[N] bits3;
    ubyte[N] bits4;
    bits4[0..n] = bits3[0..n] = def();
    ghi(bits3.ptr);
    ghi(bits4.ptr);
}

////////////////////////////////////////////////////////////////////////


/* Test all the cases of uses of LEA for multiplication by a constant
 */

T testlea(uint C, T)(T x, T y)
{
    y = y * C;          // cdmul()
    x *= C;             // cdmulass()
    return x + y;
}

void testleax(uint C)(uint X, uint Y)
{
    assert(testlea!C(X,Y) == C * (X + Y));
    assert(testlea!C(cast(long)X,cast(long)Y) == cast(long)C*X + cast(long)C*Y);
}

void testMulLea()
{
    testleax!3(10,11);
    testleax!5(10,11);
    testleax!6(10,11);
    testleax!9(10,11);

    testleax!10(10,11);
    testleax!12(10,11);
    testleax!18(10,11);
    testleax!20(10,11);
    testleax!24(10,11);
    testleax!36(10,11);
    testleax!40(10,11);
    testleax!72(10,11);

    testleax!37(10,11);
    testleax!74(10,11);
    testleax!13(10,11);
    testleax!26(10,11);
}

////////////////////////////////////////////////////////////////////////

/* Test *= of register pair
 */

void testMulAssPair()
{
    static ulong pow(ulong x, int m)
    {
        ulong v = x;
        ulong p = 1;
        while (1)
        {
            if (m & 1)
                p *= v;
            m >>= 1;
            if (!m)
                break;
            v *= v;
        }
        return p;
    }

    enum ulong e_10_pow_19 = 10uL^^19;
    assert(e_10_pow_19 == pow(10uL, 19));
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=21038

const(wchar)* x21038 = "xz";
const(dchar)* name21038 = "abcd";

void test21038()
{
    assert((cast(size_t)    x21038) % wchar.sizeof == 0);
    assert((cast(size_t) name21038) % dchar.sizeof == 0);
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=21325

real f21325(const real x) pure @safe nothrow @nogc
{
    return (x != 0.0L) ? x : real.nan;
}

void test21325() @safe
{
    ulong x = 0uL;
    while(true)
    {
        const y = f21325(x); // should set y to real.nan

        assert(y != y);

        if (++x)
            return; // good
    }
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=16274

extern(C) int pair(short a, ushort b, byte c, ubyte d);

struct S
{
    // provide alternate implementation of .pair()
    pragma(mangle, "pair")
    extern(C) static void pair(int a, int b, int c, int d)
    {
        //printf("%d %d %d %d\n", a, b, c, d);
        assert(a == -1);
        assert(b == 2);
        assert(c == -3);
        assert(d == 4);
    }
}

void test16274()
{
    version (X86_64)
        pair(-1, 2, -3, 4);
    version (X86)
        pair(-1, 2, -3, 4);
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=16268

void test16268()
{
    static void f(byte x)
    {
        for (byte i = 0; i <= x && i >= 0; ++i)
        {
            assert(i >= 0);
            assert(i != -1);
            //printf("%d\n", i);
        }
    }

    f(byte.max);
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=11435

void test11435a()
{
    alias T = byte;

    static void fun(T c, T b, int v)
    {
    }

    static void abc(T[] b)
    {
        fun(b[0], b[1], 0);
    }

    version(Windows)
    {
        import core.sys.windows.windows;
        auto p = VirtualAlloc(null, 4096, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    }
    else
    {
        import core.sys.posix.sys.mman;
        auto p = mmap(null, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0L);
    }
    assert(p);
    auto px = (cast(T*)(p + 4096 - 2 * T.sizeof));
    abc(px[0..2]);
}

void test11435b()
{
    import core.sys.windows.windows;
    alias T = short;

    static void fun(T c, T b, int v)
    {
    }

    static void abc(T[] b)
    {
        fun(b[0], b[1], 0);
    }

    version(Windows)
    {
        import core.sys.windows.windows;
        auto p = VirtualAlloc(null, 4096, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    }
    else
    {
        import core.sys.posix.sys.mman;
        auto p = mmap(null, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0L);
    }
    assert(p);
    auto px = (cast(T*)(p + 4096 - 2 * T.sizeof));
    abc(px[0..2]);
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=21513

struct Stuff
{
    size_t c;         // declare after items and not crash !
    ubyte[1] items;
}

void grow(ref Stuff stuff)
{
    with (stuff)
    {
        const oldCapacity = c;
        items.ptr[0..oldCapacity] = items.ptr[0..0]; // use literal 0 instead of
        items.ptr[0] = 0;                            // oldcapacity and no crash !
    }
}

void test21513()
{
    Stuff stuff;
    grow(stuff);
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=21526

double f21256(double a, double b) {
    double c = a + b;
    return c;
}

void test21256()
{
    union DX
    {
        double d;
        ulong l;
    }

    DX a, b;
    a.l = 0x4341c37937e08000;
    b.l = 0x4007ffcb923a29c7;

    DX r;
    r.d = f21256(a.d, b.d);
    //if (r.d != 0x1.1c37937e08001p+53)
        //printf("r = %A should be 0x1.1c37937e08001p+53 %A\n", r.d, 0x1.1c37937e08001p+53);
    //assert(r == 0x1.1c37937e08001p+53);

    // cannot seem to get the two to produce the same value
    assert(r.l == 0x4341c37937e08001 || // value using XMM
           r.l == 0x4341c37937e08002);  // value using x87
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=21816

bool test21816a(float t)
{
    return cast(bool)t;
}

void test21816()
{
    assert(test21816a(float.nan));
}

////////////////////////////////////////////////////////////////////////
// https://issues.dlang.org/show_bug.cgi?id=21835

struct Point21835
{
    float  f = 3.0;
    double d = 4.0;
    real   r = 5.0;
}

void test21835y()
{
    Point21835[1] arr;
    if (arr[0].f != 3.0) assert(0);
    if (arr[0].d != 4.0) assert(0);
    if (arr[0].r != 5.0) assert(0);
}

struct Point21835x
{
    float  f = 0.0;
    double d = 0.0;
    real   r = 0.0;
}

void test21835()
{
    test21835y();
    Point21835x[1] arr;
    if (arr[0].f != 0.0) assert(0);
    if (arr[0].d != 0.0) assert(0);
    if (arr[0].r != 0.0) assert(0);
}

////////////////////////////////////////////////////////////////////////

int main()
{
    // All the various integer divide tests
    testsdiv2();
    testulldiv();
    testfastudiv();
    testsldiv();
    testslmod();
    testfastdiv();
    testdivdiv();
    testdivcmp();

    testgoto();
    testswitch();
    testdo();
    testbreak();
    teststringswitch();
    teststrarg();
    test12164();
    testsizes();
    testarrayinit();
    testU();
    testbittest();
    test8658();
    test3918();
    test12051();
    testdocond();
    testnegcom();
    test11565();
    testoror();
    testbt();
    test12095(0);
    testandand();
    testor_combine();
    testshrshl();
    test13383();
    test13190();
    test13485();
    test14436();
    test10715();
    test10678();
    test7565();
    test13023(0x10_0000_0000);
    test12833();
    test9449();
    test9449x();
    test12057();
    test13784();
    test14220();
    test14829();
    test3();
    test14782();
    test14987();
    test15272();
    test15861();
    test15629();
    test4();
    test13474();
    test16699();
    test16102();
    test5();
    test6();
    testeqeqranges();
    test16189();
    test16997();
    test18315();
    test18461();
    test18730();
    test19497();
    test18794();
    testelmin();
    testfastpar();
    test20363();
    testNegConst();
    test16317();
    test20050();
    testCpStatic();
    test7();
    testbyteswap();
    testrolror();
    test20162();
    test3713();
    testsbbrex();
    test19846();
    testmemcpy();
    testMulLea();
    testMulAssPair();
    test21038();
    test21325();
    test16274();
    test16268();
    test11435a();
    test11435b();
    test21513();
    test21256();
    test21816();
    test21835();

    printf("Success\n");
    return 0;
}
