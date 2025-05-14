/*
RUN_OUTPUT:
---
Success
---
*/

extern(C) int printf(const char*, ...);

enum
{
    T_char,
    T_wchar,
    T_dchar,
    T_bit,
    T_byte,
    T_ubyte,
    T_short,
    T_ushort,
    T_int,
    T_uint,
    T_long,
    T_ulong,
}

int dotype(char x) { return T_char; }
int dotype(bool x) { return T_bit; }
int dotype(byte x) { return T_byte; }
int dotype(ubyte x) { return T_ubyte; }
int dotype(wchar x) { return T_wchar; }
int dotype(short x) { return T_short; }
int dotype(ushort x) { return T_ushort; }
int dotype(int x) { return T_int; }
int dotype(uint x) { return T_uint; }
int dotype(long x) { return T_long; }
int dotype(ulong x) { return T_ulong; }

void test1()
{
    /*
     * 0x7FFF             077777                  32767
     * 0x8000             0100000                 32768
     * 0xFFFF             0177777                 65535
     * 0x10000            0200000                 65536
     * 0x7FFFFFFF         017777777777            2147483647
     * 0x80000000         020000000000            2147483648
     * 0xFFFFFFFF         037777777777            4294967295
     * 0x100000000        040000000000            4294967296
     * 0x7FFFFFFFFFFFFFFF 0777777777777777777777  9223372036854775807
     * 0x8000000000000000 01000000000000000000000 9223372036854775808
     * 0xFFFFFFFFFFFFFFFF 01777777777777777777777 18446744073709551615
     */

    assert(dotype(1) == T_int);

    /***************** Hexadecimal ***********************/

    assert(dotype(0) == T_int);
    assert(dotype(0x7FFF) == T_int);
    assert(dotype(0x8000) == T_int);
    assert(dotype(0xFFFF) == T_int);
    assert(dotype(0x10000) == T_int);
    assert(dotype(0x7FFFFFFF) == T_int);
    assert(dotype(0x80000000) == T_uint);
    assert(dotype(0xFFFFFFFF) == T_uint);
    assert(dotype(0x100000000) == T_long);
    assert(dotype(0x7FFFFFFFFFFFFFFF) == T_long);
    assert(dotype(0x8000000000000000) == T_ulong);
    assert(dotype(0xFFFFFFFFFFFFFFFF) == T_ulong);

    assert(dotype(0u) == T_uint);
    assert(dotype(0x7FFFu) == T_uint);
    assert(dotype(0x8000u) == T_uint);
    assert(dotype(0xFFFFu) == T_uint);
    assert(dotype(0x10000u) == T_uint);
    assert(dotype(0x7FFFFFFFu) == T_uint);
    assert(dotype(0x80000000u) == T_uint);
    assert(dotype(0xFFFFFFFFu) == T_uint);
    assert(dotype(0x100000000u) == T_ulong);
    assert(dotype(0x7FFFFFFFFFFFFFFFu) == T_ulong);
    assert(dotype(0x8000000000000000u) == T_ulong);
    assert(dotype(0xFFFFFFFFFFFFFFFFu) == T_ulong);

    assert(dotype(0L) == T_long);
    assert(dotype(0x7FFFL) == T_long);
    assert(dotype(0x8000L) == T_long);
    assert(dotype(0xFFFFL) == T_long);
    assert(dotype(0x10000L) == T_long);
    assert(dotype(0x7FFFFFFFL) == T_long);
    assert(dotype(0x80000000L) == T_long);
    assert(dotype(0xFFFFFFFFL) == T_long);
    assert(dotype(0x100000000L) == T_long);
    assert(dotype(0x7FFFFFFFFFFFFFFFL) == T_long);
    assert(dotype(0x8000000000000000L) == T_ulong);
    assert(dotype(0xFFFFFFFFFFFFFFFFL) == T_ulong);

    assert(dotype(0uL) == T_ulong);
    assert(dotype(0x7FFFuL) == T_ulong);
    assert(dotype(0x8000uL) == T_ulong);
    assert(dotype(0xFFFFuL) == T_ulong);
    assert(dotype(0x10000uL) == T_ulong);
    assert(dotype(0x7FFFFFFFuL) == T_ulong);
    assert(dotype(0x80000000uL) == T_ulong);
    assert(dotype(0xFFFFFFFFuL) == T_ulong);
    assert(dotype(0x100000000uL) == T_ulong);
    assert(dotype(0x7FFFFFFFFFFFFFFFuL) == T_ulong);
    assert(dotype(0x8000000000000000uL) == T_ulong);
    assert(dotype(0xFFFFFFFFFFFFFFFFuL) == T_ulong);

    /***************** Decimal ***********************/

    assert(dotype(0) == T_int);
    assert(dotype(32767) == T_int);
    assert(dotype(32768) == T_int);
    assert(dotype(65535) == T_int);
    assert(dotype(65536) == T_int);
    assert(dotype(2147483647) == T_int);
    assert(dotype(2147483648) == T_long);
    assert(dotype(4294967295) == T_long);
    assert(dotype(4294967296) == T_long);
    assert(dotype(9223372036854775807) == T_long);
    //assert(dotype(9223372036854775808) == T_long);
    //assert(dotype(18446744073709551615) == T_ulong);

    assert(dotype(0u) == T_uint);
    assert(dotype(32767u) == T_uint);
    assert(dotype(32768u) == T_uint);
    assert(dotype(65535u) == T_uint);
    assert(dotype(65536u) == T_uint);
    assert(dotype(2147483647u) == T_uint);
    assert(dotype(2147483648u) == T_uint);
    assert(dotype(4294967295u) == T_uint);
    assert(dotype(4294967296u) == T_ulong);
    assert(dotype(9223372036854775807u) == T_ulong);
    assert(dotype(9223372036854775808u) == T_ulong);
    assert(dotype(18446744073709551615u) == T_ulong);

    assert(dotype(0L) == T_long);
    assert(dotype(32767L) == T_long);
    assert(dotype(32768L) == T_long);
    assert(dotype(65535L) == T_long);
    assert(dotype(65536L) == T_long);
    assert(dotype(2147483647L) == T_long);
    assert(dotype(2147483648L) == T_long);
    assert(dotype(4294967295L) == T_long);
    assert(dotype(4294967296L) == T_long);
    assert(dotype(9223372036854775807L) == T_long);
    //assert(dotype(9223372036854775808L) == T_ulong);
    //assert(dotype(18446744073709551615L) == T_ulong);

    assert(dotype(0uL) == T_ulong);
    assert(dotype(32767uL) == T_ulong);
    assert(dotype(32768uL) == T_ulong);
    assert(dotype(65535uL) == T_ulong);
    assert(dotype(65536uL) == T_ulong);
    assert(dotype(2147483647uL) == T_ulong);
    assert(dotype(2147483648uL) == T_ulong);
    assert(dotype(4294967295uL) == T_ulong);
    assert(dotype(4294967296uL) == T_ulong);
    assert(dotype(9223372036854775807uL) == T_ulong);
    assert(dotype(9223372036854775808uL) == T_ulong);
    assert(dotype(18446744073709551615uL) == T_ulong);
}

void test2()
{
    ulong[] a = [ 2_463_534_242UL ];

    foreach(e; a)
        assert(e == 2_463_534_242UL);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=13907

void f13907_1(wchar[1] a) {}
void f13907_2(wchar[2] a) {}
void f13907_3(wchar[3] a) {}

auto f13907_12(char[1]) { return 1; }
auto f13907_12(char[2]) { return 2; }

auto f13907_123(char[1]) { return 1; }
auto f13907_123(char[2]) { return 2; }
auto f13907_123(char[3]) { return 3; }
auto f13907_123(const(char)[]) { return 0; }

void test13907()
{
    static assert(!__traits(compiles, { f13907_1("\U00010000"w); }));
    static assert(!__traits(compiles, { f13907_1("\U00010000" ); }));
    f13907_2("\U00010000"w);
    f13907_2("\U00010000");
    f13907_3("\U00010000"w);    // Re-enable implicit length extension, from https://issues.dlang.org/show_bug.cgi?id=13999
    f13907_3("\U00010000" );    // Re-enable implicit length extension, from https://issues.dlang.org/show_bug.cgi?id=13999

    assert(f13907_12("a") == 1);
    assert(f13907_12("ab") == 2);
    static assert(!__traits(compiles, { f13907_12("abc"); }));

    assert(f13907_123("a") == 1);
    assert(f13907_123("ab") == 2);
    assert(f13907_123("abc") == 3);
    assert(f13907_123("abcd") == 0);

    // regression tests for the lengthen behavior in initializer
    enum const(char*) p = "hello world";
    static assert(!__traits(compiles, { static   char[5] a = "hello world"; }));  // truncation is not allowed
    static assert(!__traits(compiles, { static  void[20] a = "hello world"; }));
    static assert(!__traits(compiles, { static   int[20] a = "hello world"; }));
    static assert(!__traits(compiles, { static  char[20] a = "hello world"w; }));
    static assert(!__traits(compiles, { static wchar[20] a = "hello world"d; }));
    static assert(!__traits(compiles, { static dchar[20] a = "hello world"c; }));
    static assert(!__traits(compiles, { static  char[20] a = p; }));
    static  char[20] csa = "hello world";  // extending is allowed
    static wchar[20] wsa = "hello world";  // ok
    static dchar[20] dsa = "hello world";  // ok

    // https://issues.dlang.org/show_bug.cgi?id=13966
    string[1][] arr;
    arr ~= ["class"];
    enum immutable(char[5]) sarrstr = "class";
    arr ~= [sarrstr];

    // https://issues.dlang.org/show_bug.cgi?id=13999
    string[dchar[2]] aa13999 = ["あ": "bar"];
    assert(aa13999["あ"] == "bar");
    dchar[2] key13999 = "あ";
    assert(key13999[0] == 'あ');
    assert(key13999[1] == '\0');
    assert(aa13999[key13999] == "bar");
}

ulong op12950(ulong v){return v + 12950;}

void test12950()
{
    assert(0x00_00_00_01.op12950() == 12951);
    assert(0x00_00_00_01UL.op12950() == 12951);
    assert(0b00_00_00_01.op12950() == 12951);
    assert(0b00_00_00_01UL.op12950() == 12951);
}

void testHexstring()
{
    static immutable uint[] x = cast(immutable uint[]) x"FFAADDEE"d;
    static assert(x[0] == 0xFFAADDEE);
    assert(x[0] == 0xFFAADDEE);

    static immutable ulong[] y = x"1122334455667788AABBCCDDEEFF0099";
    static assert(y[0] == 0x1122334455667788);
    static assert(y[1] == 0xAABBCCDDEEFF0099);
    assert(y[0] == 0x1122334455667788);
    assert(y[1] == 0xAABBCCDDEEFF0099);

    immutable long[] c = x"1122334455667788AABBCCDDEEFF0099";
    assert(c[0] == 0x1122334455667788);
    assert(c[1] == 0xAABBCCDDEEFF0099);

    // Test that mangling of StringExp with size 8 is the same as array literal mangling:
    void f(immutable ulong[] a)() {}
    static assert(f!y.mangleof == f!([0x1122334455667788, 0xAABBCCDDEEFF0099]).mangleof);

    // Test printing StringExp with size 8
    enum toStr(immutable ulong[] v) = v.stringof;
    static assert(toStr!y == `x"1122334455667788AABBCCDDEEFF0099"`);

    // Hex string postfixes
    // https://issues.dlang.org/show_bug.cgi?id=24363
    wstring wStr = x"AA BB CC DD"w;
    immutable int[] dStr = x"AA BB CC DD"d;
    assert(wStr[0] == 0xAABB);
    assert(wStr[1] == 0xCCDD);
    assert(dStr[0] == 0xAABBCCDD);

    // Test sliceCmpStringWithArray with size 8
    static immutable ulong[] z0 = cast(immutable ulong[]) x"1111 1111 1111 1111 0000 000F 0000 0000";
    static immutable ulong[] z1 = [0x1111_1111_1111_1111, 0x0000_000E_0000_0000];
    static assert(z0 !is z1);

    // https://github.com/dlang/dmd/issues/20635
    f20635(cast(ubyte[]) x"00");
    f20635(cast(const ubyte[]) x"00");
    f20635(cast(immutable ubyte[]) x"00");
}

void f20635(const ubyte[] value){}
void f20635(const string value){}

/***************************************************/

int main()
{
    test1();
    test2();
    test13907();
    test12950();
    testHexstring();

    printf("Success\n");
    return 0;
}
