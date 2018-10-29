/*
REQUIRED_ARGS: -mcpu=native
PERMUTE_ARGS: -O -inline
*/

import core.stdc.stdio;

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
    printf("%d\n", Foo1.sizeof);
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

U f = { b:3, d:2, a:1 };

void testU()
{
    assert(f.b == 3);
    assert(f.d == 2);
    assert(f.c == 2);
    assert(f.a == 1);
    assert(f.sizeof == 16);
    assert(U.sizeof == 16);
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
            printf("[%d] %lld / %lld = %lld, should be %lld\n",
                vectors[i][0], vectors[i][1], q, vectors[i][2]);

        ulong r = vectors[i][0] % vectors[i][1];
        if (r != vectors[i][3])
            printf("[%d] %lld %% %lld = %lld, should be %lld\n",
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
        printf("%f\n", u );
        return t && u;
}

bool test3918b( real t, float u )
{
        printf("%f\n", t );
        return t && u;
}

void test3918()
{
        assert(test3918a(float.nan, real.nan));
        assert(test3918b(real.nan, float.nan));
}

////////////////////////////////////////////////////////////////////////


int div10(int x)
{
    return x / 10;
}

int div14(int x)
{
    return x / 14;
}

int div14007(int x)
{
    return x / 14007;
}

int mod10(int x)
{
    return x % 10;
}

int mod14(int x)
{
    return x % 14;
}

int mod14007(int x)
{
    return x % 14007;
}

int remquo10(int x)
{
    return (x / 10) | (x % 10);
}

int remquo14(int x)
{
    return (x / 14) | (x % 14);
}

int remquo14007(int x)
{
    return (x / 14007) | (x % 14007);
}

////////////////////

int mdiv10(int x)
{
    return x / -10;
}

int mdiv14(int x)
{
    return x / -14;
}

int mdiv14007(int x)
{
    return x / -14007;
}

int mmod10(int x)
{
    return x % -10;
}

int mmod14(int x)
{
    return x % -14;
}

int mmod14007(int x)
{
    return x % -14007;
}

int mremquo10(int x)
{
    return (x / -10) | (x % -10);
}

int mremquo14(int x)
{
    return (x / -14) | (x % -14);
}

int mremquo14007(int x)
{
    return (x / -14007) | (x % -14007);
}

////////////////////


long ldiv10(long x)
{
    return x / 10;
}

long ldiv14(long x)
{
    return x / 14;
}

long ldiv14007(long x)
{
    return x / 14007;
}

long lmod10(long x)
{
    return x % 10;
}

long lmod14(long x)
{
    return x % 14;
}

long lmod14007(long x)
{
    return x % 14007;
}

long lremquo10(long x)
{
    return (x / 10) | (x % 10);
}

long lremquo14(long x)
{
    return (x / 14) | (x % 14);
}

long lremquo14007(long x)
{
    return (x / 14007) | (x % 14007);
}


////////////////////


long mldiv10(long x)
{
    return x / -10;
}

long mldiv14(long x)
{
    return x / -14;
}

long mldiv14007(long x)
{
    return x / -14007;
}

long mlmod10(long x)
{
    return x % -10;
}

long mlmod14(long x)
{
    return x % -14;
}

long mlmod14007(long x)
{
    return x % -14007;
}

long mlremquo10(long x)
{
    return (x / -10) | (x % -10);
}

long mlremquo14(long x)
{
    return (x / -14) | (x % -14);
}

long mlremquo14007(long x)
{
    return (x / -14007) | (x % -14007);
}



void testfastdiv()
{
  {
    static int x10 = 10;
    static int x14 = 14;
    static int x14007 = 14007;

    int u = 10000;
    int r;
    r = div10(u);  assert(r == u/x10);
    r = div14(u);  assert(r == u/x14);
    r = div14007(u);  assert(r == u/x14007);
    r = mod10(u);  assert(r == u%x10);
    r = mod14(u);  assert(r == u%x14);
    r = mod14007(u);  assert(r == u%x14007);
    r = remquo10(u);  assert(r == ((u/x10)|(u%x10)));
    r = remquo14(u);  assert(r == ((u/x14)|(u%x14)));
    r = remquo14007(u);  assert(r == ((u/x14007)|(u%x14007)));
  }
  {
    static int t10 = -10;
    static int t14 = -14;
    static int t14007 = -14007;

    int u = 10000;
    int r;
    r = mdiv10(u);  assert(r == u/t10);
    r = mdiv14(u);  assert(r == u/t14);
    r = mdiv14007(u);  assert(r == u/t14007);
    r = mmod10(u);  assert(r == u%t10);
    r = mmod14(u);  assert(r == u%t14);
    r = mmod14007(u);  assert(r == u%t14007);
    r = mremquo10(u);  assert(r == ((u/t10)|(u%t10)));
    r = mremquo14(u);  assert(r == ((u/t14)|(u%t14)));
    r = mremquo14007(u);  assert(r == ((u/t14007)|(u%t14007)));
  }
  {
    static long y10 = 10;
    static long y14 = 14;
    static long y14007 = 14007;

    long u = 10000;
    long r;
    r = ldiv10(u);  assert(r == u/y10);
    r = ldiv14(u);  assert(r == u/y14);
    r = ldiv14007(u);  assert(r == u/y14007);
    r = lmod10(u);  assert(r == u%y10);
    r = lmod14(u);  assert(r == u%y14);
    r = lmod14007(u);  assert(r == u%y14007);
    r = lremquo10(u);  assert(r == ((u/y10)|(u%y10)));
    r = lremquo14(u);  assert(r == ((u/y14)|(u%y14)));
    r = lremquo14007(u);  assert(r == ((u/y14007)|(u%y14007)));
  }
  {
    static long z10 = -10;
    static long z14 = -14;
    static long z14007 = -14007;

    long u = 10000;
    long r;
    r = mldiv10(u);  assert(r == u/z10);
    r = mldiv14(u);  assert(r == u/z14);
    r = mldiv14007(u);  assert(r == u/z14007);
    r = mlmod10(u);  assert(r == u%z10);
    r = mlmod14(u);  assert(r == u%z14);
    r = mlmod14007(u);  assert(r == u%z14007);
    r = mlremquo10(u);  assert(r == ((u/z10)|(u%z10)));
    r = mlremquo14(u);  assert(r == ((u/z14)|(u%z14)));
    r = mlremquo14007(u);  assert(r == ((u/z14007)|(u%z14007)));
  }
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

struct S1
{
    cdouble val;
}

void formatTest(S1 s, double re, double im)
{
    assert(s.val.re == re);
    assert(s.val.im == im);
}

void test10639()
{
    S1 s = S1(3+2.25i);
    formatTest(s, 3, 2.25);
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

void test2()
{
    void test(cdouble v)
    {
            auto x2 = cdouble(v);
            assert(x2 == v);
    }
    test(1.2+3.4i);
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
// 14782


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
    import std.math : fabs;
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

int main()
{
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
    testulldiv();
    testbittest();
    test8658();
    testfastudiv();
    testfastdiv();
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
    test10639();
    test10715();
    test10678();
    test7565();
    test13023(0x10_0000_0000);
    test12833();
    test9449();
    test12057();
    test13784();
    test14220();
    test14829();
    test2();
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
    testdivdiv();
    test5();
    test6();
    testeqeqranges();
    printf("Success\n");
    return 0;
}
