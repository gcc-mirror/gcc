module traits_getPointerBitmap;

import core.stdc.stdio;

// version = RTInfo;
// debug = LOG;

version(RTInfo)
    import gc.rtinfo;
else
    enum bool RTInfoMark__Monitor = false; // is __monitor GC allocated?


enum bytesPerPtr = (size_t.sizeof);
enum bytesPerBitmapWord = bytesPerPtr * bytesPerPtr * 8;

template allocatedSize(T)
{
    static if (is (T == class))
        enum allocatedSize = __traits(classInstanceSize, T);
    else
        enum allocatedSize = T.sizeof;
}

bool testBit(const(size_t)* p, size_t biti)
{
    enum BITS_SHIFT = (size_t.sizeof == 8 ? 6 : 5);
    enum BITS_MASK = (bytesPerPtr - 1);

    return (p[biti >> BITS_SHIFT] & (1 << (biti & BITS_MASK))) != 0;
}

void __testType(T)(size_t[] expected)
{
    // check compile time info
    enum bits  = (T.sizeof + bytesPerPtr - 1) / bytesPerPtr;
    enum words = (T.sizeof + bytesPerBitmapWord - 1) / bytesPerBitmapWord;
    version(RTInfo)
        enum info = RTInfoImpl2!(Unqual!T); // we want the array, not the pointer
    else
        enum info = __traits(getPointerBitmap,T); // we want the array, not the pointer

    debug(LOG) writef("%-20s:", T.stringof);
    debug(LOG) writef(" CT:%s", info);
    debug(LOG) writef(" EXP:%d %s", allocatedSize!T, expected);
    assert(info[0] == allocatedSize!T);
    assert(info[1..$] == expected);
    assert(words == expected.length);

    debug(LOG) writeln();
}

///////////////////////////////////////
struct S(T, aliasTo = void)
{
    static if(!is(aliasTo == void))
    {
        aliasTo a;
        alias a this;
    }

    size_t x;
    T t = void;
    void* p;

}

template tOff(T)
{
    enum tOff = T.t.offsetof / bytesPerPtr;
}

template pOff(T)
{
    enum pOff = T.p.offsetof / bytesPerPtr;
}

///////////////////////////////////////

void _testType(T)(size_t[] expected)
{
    __testType!(T)(expected);
    __testType!(const(T))(expected);
    __testType!(immutable(T))(expected);
    version(RTInfo) {} else // Unqual does not work with shared(T[N])
        __testType!(shared(T))(expected);
}

void testType(T)(size_t[] expected)
{
    _testType!(T)(expected);

    // generate bit pattern for S!T
    assert(expected.length == 1);
    size_t[] sexp;

    sexp ~= (expected[0] << tOff!(S!T)) | (1 << pOff!((S!T)));
    _testType!(S!T)(sexp);

    // prepend Object
    sexp[0] = (expected[0] << tOff!(S!(T, Object))) | (1 << pOff!(S!(T, Object))) | 1;
    _testType!(S!(T, Object))(sexp);

    // prepend string
    sexp[0] = (expected[0] << tOff!(S!(T, string))) | (1 << pOff!(S!(T, string))) | 2; // arr ptr
    _testType!(S!(T, string))(sexp);
}

///////////////////////////////////////
alias void[2*size_t.sizeof] void2;
alias size_t[3] int3;
alias size_t*[3] pint3;
alias string[3] sint3;
alias string[3][2] sint3_2;
alias int delegate() dg;
alias int function() fn;
alias typeof(null) NullType;

// span multiple bitmap elements
struct Large
{
    size_t[30] data1;
    void* p1;
    size_t[1] val1;

    size_t[28] data2;
    void* p2;
    size_t[3] val2;

    size_t[16] data3;
    void* p3;
    size_t[15] val3;
}

class N
{
    struct Nested
    {
        // no outer for structs
        size_t x;
        void* p1;
        Large* s;

        void foo() {} // need member fnction to not be POD
    }
    class CNested
    {
        // implicit vtptr,monitor
        size_t x;
        void* p1;
        size_t y;
        // implicit outer
    }
    class CNestedDerived : CNested
    {
        size_t[3] z;
        void* p;
    }
}

union U
{
    size_t[4] data;
    Large*[] arr; // { length, ptr }

    struct
    {
        size_t d1;
        size_t d2;
        size_t d3;
        void* p;
    }
}

void testRTInfo()
{
    testType!(bool)         ([ 0b0 ]);
    testType!(ubyte)        ([ 0b0 ]);
    testType!(short)        ([ 0b0 ]);
    testType!(int)          ([ 0b0 ]);
    testType!(long)         ([ 0b00 ]);
    testType!(double)       ([ 0b00 ]);
    testType!(dg)           ([ 0b01 ]);
    testType!(fn)           ([ 0b0 ]);
    testType!(S!fn)         ([ 0b100 ]);
    testType!(NullType)     ([ 0b0 ]);
    static if (__traits(compiles, __vector(float[4])))
        testType!(__vector(float[4]))  ([ 0b00 ]);

    testType!(Object[int])       ([ 0b1 ]);
    testType!(Object[])       ([ 0b10 ]);
    testType!(string)         ([ 0b10 ]);

    testType!(int3)           ([ 0b000 ]);
    testType!(pint3)          ([ 0b111 ]);
    testType!(sint3)          ([ 0b101010 ]);
    testType!(sint3_2)        ([ 0b101010101010 ]);
    testType!(void2)          ([ 0b11 ]);
    testType!(U)              ([ 0b1010 ]);

    version(D_LP64)
        _testType!(Large)          ([ 0x1000_0000__4000_0000, 0x0001_0000 ]);
    else
        _testType!(Large)          ([ 0x4000_0000, 0x1000_0000, 0x0001_0000 ]);

    _testType!(N.CNested)     ([ 0b101000 ]);
    _testType!(N.CNestedDerived) ([ 0b1000101000 ]);

    testType!(N.Nested)       ([ 0b110 ]);

    struct SFNested
    {
        size_t[2] d;
        void* p1;
        fn f;
        // implicite outer

        void foo() {} // need member fnction to not be POD
    }

    class CFNested
    {
        // implicit vtptr,monitor
        size_t[2] d;
        void* p1;
        // implicite outer
    }

    testType!(SFNested)      ([ 0b10100 ]);
    _testType!(CFNested)     ([ 0b110000 ]);
}

void main()
{
    testRTInfo();
}
