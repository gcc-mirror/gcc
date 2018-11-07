// REQUIRED_ARGS:

// Test array bounds checking

import core.exception;
extern(C) int printf(const char*, ...);

template TypeTuple(T...) { alias T TypeTuple; }

/******************************************/

const int[10] foos = [1,2,3,4,5,6,7,8,9,10];
const int[] food   = [21,22,23,24,25,26,27,28,29,30];
const int *foop    = cast(int*) foos;

static int x = 2;

int index()
{
    return x++;
}

int tests(int i)
{
    return foos[index()];
}

int testd(int i)
{
    return food[index()];
}

int testp(int i)
{
    return foop[i];
}

const(int)[] slices(int lwr, int upr)
{
    return foos[lwr .. upr];
}

const(int)[] sliced(int lwr, int upr)
{
    return food[lwr .. upr];
}

const(int)[] slicep(int lwr, int upr)
{
    return foop[lwr .. upr];
}

void test1()
{
    int i;

    i = tests(0);
    assert(i == 3);

    i = testd(0);
    assert(i == 24);

    i = testp(1);
    assert(i == 2);

    x = 10;
    try
    {
        i = tests(0);
    }
    catch (RangeError a)
    {
        i = 73;
    }
    assert(i == 73);

    x = -1;
    try
    {
        i = testd(0);
    }
    catch (RangeError a)
    {
        i = 37;
    }
    assert(i == 37);

    const(int)[] r;

    r = slices(3,5);
    assert(r[0] == foos[3]);
    assert(r[1] == foos[4]);

    r = sliced(3,5);
    assert(r[0] == food[3]);
    assert(r[1] == food[4]);

    r = slicep(3,5);
    assert(r[0] == foos[3]);
    assert(r[1] == foos[4]);

    try
    {
        i = 7;
        r = slices(5,3);
    }
    catch (RangeError a)
    {
        i = 53;
    }
    assert(i == 53);

    try
    {
        i = 7;
        r = slices(5,11);
    }
    catch (RangeError a)
    {
        i = 53;
    }
    assert(i == 53);

    try
    {
        i = 7;
        r = sliced(5,11);
    }
    catch (RangeError a)
    {
        i = 53;
    }
    assert(i == 53);

    try
    {
        i = 7;
        r = slicep(5,3);
    }
    catch (RangeError a)
    {
        i = 53;
    }
    assert(i == 53);

    // Take side effects into account
    x = 1;
    r = foos[index() .. 3];
    assert(x == 2);
    assert(r[0] == foos[1]);
    assert(r[1] == foos[2]);

    r = foos[1 .. index()];
    assert(r.length == 1);
    assert(x == 3);
    assert(r[0] == foos[1]);

    x = 1;
    r = food[index() .. 3];
    assert(x == 2);
    assert(r[0] == food[1]);
    assert(r[1] == food[2]);

    r = food[1 .. index()];
    assert(r.length == 1);
    assert(x == 3);
    assert(r[0] == food[1]);

    x = 1;
    r = foop[index() .. 3];
    assert(x == 2);
    assert(r[0] == foop[1]);
    assert(r[1] == foop[2]);

    r = foop[1 .. index()];
    assert(r.length == 1);
    assert(x == 3);
    assert(r[0] == foop[1]);
}

/******************************************/
// 13976

void test13976()
{
    int[] da = new int[](10);
    int[10] sa;
    size_t l = 0;               // upperInRange
    size_t u = 9;               // | lowerLessThan
                                // | |  check code
    { auto s = da[l .. u];   }  // 0 0  (u <= 10 && l <= u  )
    { auto s = da[1 .. u];   }  // 0 0  (u <= 10 && l <= u  )
    { auto s = da[l .. 10];  }  // 0 0  (u <= 10 && l <= u  )
    { auto s = da[1 .. u%5]; }  // 0 0  (u <= 10 && l <= u%5)

    { auto s = da[l .. u];   }  // 0 0  (u   <= 10 && l <= u)
    { auto s = da[0 .. u];   }  // 0 1  (u   <= 10          )
    { auto s = da[l .. 10];  }  // 0 0  (u   <= 10 && l <= u)
    { auto s = da[0 .. u%5]; }  // 0 1  (u%5 <= 10          )

    { auto s = sa[l .. u];   }  // 0 0  (u <= 10 && l <= u  )
    { auto s = sa[1 .. u];   }  // 0 0  (u <= 10 && l <= u  )
    { auto s = sa[l .. 10];  }  // 1 0  (           l <= u  )
    { auto s = sa[1 .. u%5]; }  // 1 0  (           l <= u%5)

    { auto s = sa[l .. u];   }  // 0 0  (u <= 10 && l <= u )
    { auto s = sa[0 .. u];   }  // 0 1  (u <= 10           )
    { auto s = sa[l .. 10];  }  // 1 0  (           l <= 10)
    { auto s = sa[0 .. u%5]; }  // 1 1  NULL

    int* p = new int[](10).ptr;
    { auto s = p[0 .. u];    }  // 1 1  NULL
    { auto s = p[l .. u];    }  // 1 0  (l <= u)
    { auto s = p[0 .. u%5];  }  // 1 1  NULL
    { auto s = p[1 .. u%5];  }  // 1 0  (l <= u%5)
}

/******************************************/
// 3652

void test3652()
{
    int foo(int[4] x)
    {
        return x[0] + x[1] * x[2] - x[3];
    }

    int[] xs = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15];

    // simple case
    foo(xs[0 .. 4]);

  version(none)
  {
    // Need deformation of formula and detection of base point
    int x = 0;
    int y = 0;
    foreach (i; 0 .. 4)
    {
        x += foo(xs[i .. i + 4]);
        y += foo(xs[(i*4+10)/2 .. (i*8>>1)/2+9]);
        // lwr = (i*4 + 10)/2 = i*4/2 + 10/2            = (i*2+5)
        // upr = (i*8>>1)/2 + 5 = (i*4/2) + 5 = i*2 + 9 = (i*2+5) + 4
    }
    assert(x == (0,1,2,3) + (1,2,3, 4) + (2, 3, 4, 5) + ( 3, 4, 5, 6));
    assert(y == (5,6,7,8) + (7,8,9,10) + (9,10,11,12) + (11,12,13,14));
  }
}

void test3652a() @safe
{
    string str = "aaaabbbbccccdddd";
    //printf("str.ptr = %p\n", str.ptr);

    void foo(ref const(char)[16] buf)
    {
        //printf("buf.ptr = %p\n", buf.ptr);
        assert(buf.ptr is str.ptr);
    }

    // can check length at runtime
    assert(str.length == 16);

    // compiler can check the length of string literal, so
    // conversion from immutable(char)[] to ref const(char)[16] is allowed;
    static assert(__traits(compiles, foo("aaaabbbbccccdddd")));

    // OK, correctly rejected by the compiler.
    static assert(!__traits(compiles, foo(str[])));

    // Ugly, furthermore does not work in safe code!
    //foo(*cast(const(char)[16]*)(str[0..16].ptr));

    // New: compiler can check the length of slice, but currently it is not allowed.
    enum size_t m = 0;
    size_t calc(){ return 0; }
    foo(str[0 .. 16]);
    foo(str[m .. 16]);
  //foo(str[calc() .. 16]); // with CTFE

    // If boundaries cannot be calculated in compile time, it's rejected.
    size_t n;
    size_t calc2(){ return n; }
    static assert(!__traits(compiles, foo(str[n .. 16])));
    static assert(!__traits(compiles, foo(str[calc2() .. 16])));

    void hoo1(size_t dim)(char[dim]) { static assert(dim == 2); }
    void hoo2(char[2]) {}
    void hoo3(size_t dim)(ref char[dim]) {}
    void hoo4(ref char[2]) {}
    hoo1(str[0 .. 2]);
    hoo2(str[0 .. 2]);
    static assert(!__traits(compiles, hoo3(str[0 .. 2])));
    static assert(!__traits(compiles, hoo4(str[0 .. 2])));
}
void test3652b() @safe
{
    int[] da = [1,2,3,4,5];

    void bar(int[3] sa1, ref int[3] sa2)
    {
        assert(sa1 == [1,2,3] && sa1.ptr !is da.ptr);
        assert(sa2 == [1,2,3] && sa2.ptr  is da.ptr);
    }
    bar(da[0..3], da[0..3]);
    static assert(!__traits(compiles, bar(da[0..4], da[0..4])));

    void baz1(T)(T[3] sa1, ref T[3] sa2)
    {
        assert(sa1 == [1,2,3] && sa1.ptr !is da.ptr);
        assert(sa2 == [1,2,3] && sa2.ptr  is da.ptr);
    }
    void baz2(T, size_t dim)(T[dim] sa1, ref T[dim] sa2, size_t result)
    {
        assert(dim == result);
        static if (dim == 3)
        {
            assert(sa1 == [1,2,3] && sa1.ptr !is da.ptr);
            assert(sa2 == [1,2,3] && sa2.ptr  is da.ptr);
        }
        else
        {
            assert(sa1 == [1,2,3,4] && sa1.ptr !is da.ptr);
            assert(sa2 == [1,2,3,4] && sa2.ptr  is da.ptr);
        }
    }
    baz1(da[0..3], da[0..3]);
    static assert(!__traits(compiles, baz1(da[0..4], da[0..4])));
    baz2(da[0..3], da[0..3], 3);
    baz2(da[0..4], da[0..4], 4);

    void hoo1(size_t dim)(int[dim]) { static assert(dim == 2); }
    void hoo2(int[2]) {}
    void hoo3(size_t dim)(ref int[dim]) {}
    void hoo4(ref int[2]) {}
    hoo1(da.idup[0 .. 2]);
    hoo2(da.idup[0 .. 2]);
    static assert(!__traits(compiles, hoo3(da.idup[0 .. 2])));
    static assert(!__traits(compiles, hoo4(da.idup[0 .. 2])));
}

/**********************************/
// 9654

auto foo9654a(ref           char[8] str) { return str; }
auto foo9654b(ref     const char[8] str) { return str; }
auto foo9654c(ref immutable char[8] str) { return str; }
static assert(!is(typeof(foo9654a("testinfo"))));
static assert( is(typeof(foo9654b("testinfo")) ==     const char[8]));
static assert( is(typeof(foo9654c("testinfo")) == immutable char[8]));

auto bar9654a(T)(ref           T[8] str) { return str; static assert(is(T == immutable char)); }
auto bar9654b(T)(ref     const T[8] str) { return str; static assert(is(T ==           char)); }
auto bar9654c(T)(ref immutable T[8] str) { return str; static assert(is(T ==           char)); }
static assert( is(typeof(bar9654a("testinfo")) == immutable char[8]));
static assert( is(typeof(bar9654b("testinfo")) ==     const char[8]));
static assert( is(typeof(bar9654c("testinfo")) == immutable char[8]));

auto baz9654a(T, size_t dim)(ref           T[dim] str) { return str; static assert(is(T == immutable char)); }
auto baz9654b(T, size_t dim)(ref     const T[dim] str) { return str; static assert(is(T ==           char)); }
auto baz9654c(T, size_t dim)(ref immutable T[dim] str) { return str; static assert(is(T ==           char)); }
static assert( is(typeof(baz9654a("testinfo")) == immutable char[8]));
static assert( is(typeof(baz9654b("testinfo")) ==     const char[8]));
static assert( is(typeof(baz9654c("testinfo")) == immutable char[8]));

/******************************************/
// 9712

auto func9712(T)(T[2] arg) { return arg; }
static assert(is(typeof(func9712([1,2])) == int[2]));

auto deduceLength9712(T,size_t n)(T[n] a) { return a; }
static assert(is(typeof(deduceLength9712([1,2,3])) == int[3]));

/******************************************/
// 9743

void test9743()
{
    //    +-Char
    //    |+-Immutable or Const or Mutable
    //    ||+-Value or Ref
    //    |||+-Function                           or +-Template
    void fCIVF(    immutable  char[4]) {}   void fCIVT()(    immutable  char[4]) {}
    void fCCVF(        const  char[4]) {}   void fCCVT()(        const  char[4]) {}
    void fCMVF(               char[4]) {}   void fCMVT()(               char[4]) {}
    void fCIRF(ref immutable  char[4]) {}   void fCIRT()(ref immutable  char[4]) {}
    void fCCRF(ref     const  char[4]) {}   void fCCRT()(ref     const  char[4]) {}
    void fCMRF(ref            char[4]) {}   void fCMRT()(ref            char[4]) {}
    alias fcOK = TypeTuple!(fCIVF, fCIVT, fCCVF, fCCVT, fCMVF, fCMVT, fCIRF, fCIRT, fCCRF, fCCRT);
    foreach (f; fcOK)                                   f("1234" )   ;
    foreach (f; fcOK)                                   f("1234"c)   ;
    foreach (f; fcOK) static assert(!__traits(compiles, f("1234"w) ));
    foreach (f; fcOK) static assert(!__traits(compiles, f("1234"d) ));
    alias fcNG = TypeTuple!(fCMRF, fCMRT);  // cannot hold immutable data by mutable ref
    foreach (f; fcNG) static assert(!__traits(compiles, f("1234" ) ));
    foreach (f; fcNG) static assert(!__traits(compiles, f("1234"c) ));
    foreach (f; fcNG) static assert(!__traits(compiles, f("1234"w) ));
    foreach (f; fcNG) static assert(!__traits(compiles, f("1234"d) ));

    //    +-Wchar
    void fWIVF(    immutable wchar[4]) {}   void fWIVT()(    immutable wchar[4]) {}
    void fWCVF(        const wchar[4]) {}   void fWCVT()(        const wchar[4]) {}
    void fWMVF(              wchar[4]) {}   void fWMVT()(              wchar[4]) {}
    void fWIRF(ref immutable wchar[4]) {}   void fWIRT()(ref immutable wchar[4]) {}
    void fWCRF(ref     const wchar[4]) {}   void fWCRT()(ref     const wchar[4]) {}
    void fWMRF(ref           wchar[4]) {}   void fWMRT()(ref           wchar[4]) {}
    alias fwOK = TypeTuple!(fWIVF, fWIVT, fWCVF, fWCVT, fWMVF, fWMVT, fWIRF, fWIRT, fWCRF, fWCRT);
    foreach (f; fwOK)                                   f("1234" )   ;
    foreach (f; fwOK) static assert(!__traits(compiles, f("1234"c) ));
    foreach (f; fwOK)                                   f("1234"w)   ;
    foreach (f; fwOK) static assert(!__traits(compiles, f("1234"d) ));
    alias fwNG = TypeTuple!(fWMRF, fWMRT);  // cannot hold immutable data by mutable ref
    foreach (f; fwNG) static assert(!__traits(compiles, f("1234" ) ));
    foreach (f; fwNG) static assert(!__traits(compiles, f("1234"c) ));
    foreach (f; fwNG) static assert(!__traits(compiles, f("1234"w) ));
    foreach (f; fwNG) static assert(!__traits(compiles, f("1234"d) ));

    //    +-Dchar
    void fDIVF(    immutable dchar[4]) {}   void fDIVT()(    immutable dchar[4]) {}
    void fDCVF(        const dchar[4]) {}   void fDCVT()(        const dchar[4]) {}
    void fDMVF(              dchar[4]) {}   void fDMVT()(              dchar[4]) {}
    void fDIRF(ref immutable dchar[4]) {}   void fDIRT()(ref immutable dchar[4]) {}
    void fDCRF(ref     const dchar[4]) {}   void fDCRT()(ref     const dchar[4]) {}
    void fDMRF(ref           dchar[4]) {}   void fDMRT()(ref           dchar[4]) {}
    alias fdOK = TypeTuple!(fDIVF, fDIVT, fDCVF, fDCVT, fDMVF, fDMVT, fDIRF, fDIRT, fDCRF, fDCRT);
    foreach (f; fdOK)                                   f("1234" )   ;
    foreach (f; fdOK) static assert(!__traits(compiles, f("1234"c) ));
    foreach (f; fdOK) static assert(!__traits(compiles, f("1234"w) ));
    foreach (f; fdOK)                                   f("1234"d)   ;
    alias fdNG = TypeTuple!(fDMRF, fDMRT);  // cannot hold immutable data by mutable ref
    foreach (f; fdNG) static assert(!__traits(compiles, f("1234" ) ));
    foreach (f; fdNG) static assert(!__traits(compiles, f("1234"c) ));
    foreach (f; fdNG) static assert(!__traits(compiles, f("1234"w) ));
    foreach (f; fdNG) static assert(!__traits(compiles, f("1234"d) ));
}

/******************************************/
// 9747

void foo9747A(T)(T[4]) {}
void foo9747C(size_t dim)(char[dim]) {}
void foo9747W(size_t dim)(wchar[dim]) {}
void foo9747D(size_t dim)(dchar[dim]) {}

void test9747()
{
    foo9747A("abcd"c);
    foo9747A("abcd"w);
    foo9747A("abcd"d);
    foo9747C("abcd"c);
    foo9747W("abcd"w);
    foo9747D("abcd"d);
}

/******************************************/
// 12876

void test12876()
{
    void foo(int[4] b) {}
    void bar(size_t n)(int[n] c) { static assert(n == 4); }

    int[5] a;
    foo(a[1 .. $]); // OK
    bar(a[1 .. $]); // OK <- Error
}

/******************************************/
// 13775

void test13775()
{
    ubyte[4] ubytes = [1,2,3,4];

    // CT-known slicing (issue 3652)
    auto ok1 = cast(ubyte[2]) ubytes[0 .. 2];
    assert(ok1 == [1, 2]);

    // CT-known slicing with implicit conversion of SliceExp::e1 (issue 13154)
    enum double[] arr = [1.0, 2.0, 3.0];
    auto ok2 = cast(float[2]) [1.0, 2.0, 3.0][0..2];
    auto ok3 = cast(float[2]) arr[1..3];    // currently this is accepted
    assert(ok2 == [1f, 2f]);
    assert(ok3 == [2f, 3f]);

    // CT-known slicing with type coercing (issue 13775)
    auto ok4 = cast( byte[2]) ubytes[0 .. 2];   // CT-known slicing + type coercing
    auto ok5 = cast(short[1]) ubytes[0 .. 2];   // CT-known slicing + type coercing
    assert(ok4 == [1, 2]);
    version(LittleEndian) assert(ok5 == [0x0201]);
    version(   BigEndian) assert(ok5 == [0x0102]);
}

/******************************************/

int main()
{
    test1();
    test13976();
    test3652();
    test3652a();
    test3652b();
    test9743();
    test9747();
    test13775();

    printf("Success\n");
    return 0;
}
