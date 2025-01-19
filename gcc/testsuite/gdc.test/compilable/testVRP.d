// PERMUTE_ARGS: -O -inline

// Test value-range propagation.
// https://issues.dlang.org/show_bug.cgi?id=3147
// https://issues.dlang.org/show_bug.cgi?id=6000
// https://issues.dlang.org/show_bug.cgi?id=5225
// https://issues.dlang.org/show_bug.cgi?id=24855

void add()
{
    byte x, y;
    short a = x + y;
}

void leftShift()
{
    byte x, y;
    short z = x << 1;
}

void leftShiftFail()
{
    {
        ubyte x, y;
        ushort z;
        static assert(!__traits(compiles, z = x << y));
        // 1 << 31 surely overflows the range of 'ushort'.
    }
    {
        ulong a, b;
        int res;
        static assert(!__traits(compiles, res = a << (b % 65U)));
    }
}

void rightShiftFail()
{
    {
        short x;
        byte y, z;
        static assert(!__traits(compiles, z = x >> y));
        // [this passes in 2.053.]
    }
    {
        ulong a, b;
        int res;
        static assert(!__traits(compiles, res = a >> (b % 65U)));
    }
}

void rightShift()
{
    ushort x;
    ubyte y = x >> 16;
}

void unsignedRightShiftFail()
{
    int x;
    ubyte y;
    static assert(!__traits(compiles, y = x >>> 2));
    // [this passes in 2.053.]
}

void subtract()
{
    ubyte x, y;
    short z = x - y;
}

void multiply()
{
    byte x, y;
    short z = x * y;
}

void subMulFail()
{
    ubyte x, y;
    ubyte z;
    static assert(!__traits(compiles, z = x - y));
    static assert(!__traits(compiles, z = x * y));
    // [these pass in 2.053.]
}

void multiplyNeg1()
{
    byte b;
    b = -1 + (b * -1);
    static assert(!__traits(compiles, b = -1 + b * ulong.max));
}

void divide()
{
    short w;
    byte y = w / 300;
}

void divideFail()
{
    short w;
    byte y;
    static assert(!__traits(compiles, y = w / -1));
    static assert(!__traits(compiles, y = y / w));

    short z;
    static assert(!__traits(compiles, z = w / z + 1));
}

void plus1Fail()
{
    byte u, v;
    static assert(!__traits(compiles, v = u + 1));
    // [these pass in 2.053.]
}

void modulus()
{
    int x;
    byte u = x % 128;
}

void modulus_bug6000a()
{
    ulong t;
    uint u = t % 16;
}

void modulus_bug6000b()
{
    long n = 10520;
    ubyte b;
    static assert(!__traits(compiles, b = n % 10));
}

void modulus2()
{
    short s;
    byte b = byte.max;
    byte c = s % b;
}

void modulus3()
{
    int i;
    short s = short.max;
    short t = i % s;
}

void modulus4()
{
    uint i;
    ushort s;
    short t;
    static assert(!__traits(compiles, t = i % s));
}

void modulus5()
{
    short a;
    byte foo = (a - short.max - 1) % 127;
}

void modulusFail()
{
    int i;
    short s;
    byte b;
    static assert(!__traits(compiles, b = i % s));
    static assert(!__traits(compiles, b = i % 257));
    // [these pass in 2.053.]
}

void bitwise()
{
    ubyte a, b, c;
    uint d;
    c = a & b;
    c = a | b;
    c = a ^ b;
    c = d & 0xff;
    // [these pass in 2.053.]
}

void bitAnd()
{
    byte c;
    int d;
    c = (0x3ff_ffffU << (0&c)) & (0x4000_0000U << (0&c));
    // the result of the above is always 0 :).
}

void bitAndTest()
{
    {
        ushort a, b;
        byte res = ((a % 7) - 6) & ((b % 7) - 6);
    }
    {
        // rhs[-128..127] outside range of lhs[0..255]
        //   -> calls byte.implicitConvTo(ubyte) => MATCH.convert
        byte a, b;
        ubyte res;

        res = cast(byte)(a + 5) & b;
        res = cast(byte)(a - 5) & b;
        res = cast(byte)(a / 5) & b;
        res = cast(byte)(a * 5) & b;
        res = cast(byte)(a % 5) & b;
    }
}

void bitOrFail()
{
    {
        ubyte c;
        static assert(!__traits(compiles, c = c | 0x100));
        // [this passes in 2.053.]
    }
    {
        byte a, b;
        ubyte res;

        static assert(!__traits(compiles, res = (a + 5) | b)); // [-128..255]
        static assert(!__traits(compiles, res = (a - 5) | b)); // [-133..127]
        static assert(!__traits(compiles, res = (a / 5) | b)); // [-128..127]
        static assert(!__traits(compiles, res = (a * 5) | b)); // [-640..639]
        static assert(!__traits(compiles, res = (a % 5) | b)); // [-128..127]
    }
}

void bitAndOr()
{
    ubyte c;
    c = (c | 0x1000) & ~0x1000;
}

void bitOrTest()
{
    {
        // Tests condition for different signs between min & max
        // ((imin.negative ^ imax.negative) == 1 && (rhs.imin.negative ^ rhs.imax.negative) == 1
        ushort a, b;
        byte res = ((a % 127) - 126) | ((b % 6) - 5);
    }
    {
        // rhs[-128..127] outside range of lhs[0..255]
        //   -> calls byte.implicitConvTo(ubyte) => MATCH.convert
        byte a, b, c;
        ubyte res;

        res = cast(byte)(a + 5) | b;
        res = cast(byte)(a - 5) | b;
        res = cast(byte)(a / 5) | b;
        res = cast(byte)(a * 5) | b;
        res = cast(byte)(a % 5) | b;
    }
}

void bitAndFail()
{
    {
        int d;
        short s;
        byte c;
        static assert(!__traits(compiles, c = d & s));
        static assert(!__traits(compiles, c = d & 256));
        // [these pass in 2.053.]
    }
    {
        byte a, b;
        ubyte res;

        static assert(!__traits(compiles, res = (a + 5) & b)); // [-128..132]
        static assert(!__traits(compiles, res = (a - 5) & b)); // [-256..127]
        static assert(!__traits(compiles, res = (a / 5) & b)); // [-128..127]
        static assert(!__traits(compiles, res = (a * 5) & b)); // [-640..635]
        static assert(!__traits(compiles, res = (a % 5) & b)); // [-128..127]
    }
}

void bitXor()
{
    {
        ushort s;
        ubyte c;
        c = (0xffff << (s & 0)) ^ 0xff00;
    }
    {
        // rhs[-128..127] outside range of lhs[0..255]
        //   -> calls byte.implicitConvTo(ubyte) => MATCH.convert
        byte a, b, c;
        ubyte res;

        res = cast(byte)(a + 5) ^ b;
        res = cast(byte)(a - 5) ^ b;
        res = cast(byte)(a / 5) ^ b;
        res = cast(byte)(a * 5) ^ b;
        res = cast(byte)(a % 5) ^ b;
    }
}

void bitXorFail()
{
    {
        byte a, b;
        ubyte res;

        static assert(!__traits(compiles, res = (a + 5) ^ b)); // [-256..255]
        static assert(!__traits(compiles, res = (a - 5) ^ b)); // [-256..255]
        static assert(!__traits(compiles, res = (a / 5) ^ b)); // [-128..127]
        static assert(!__traits(compiles, res = (a * 5) ^ b)); // [-640..1023]
        static assert(!__traits(compiles, res = (a % 5) ^ b)); // [-128..127]
    }
}

void bitComplement()
{
    int i;
    ubyte b = ~(i | ~0xff);
}

void bitComplementFail()
{
    ubyte b;
    static assert(!__traits(compiles, b = ~(b | 1)));
    // [this passes in 2.053.]
}

void negation()
{
    int x;
    byte b = -(x & 0x7);
}

void negationFail()
{
    int x;
    byte b;
    static assert(!__traits(compiles, b = -(x & 255)));
    // [this passes in 2.053.]
}

short bug5225(short a) {
    return a>>1;
}

short bug1977_comment5(byte i) {
  byte t = 1;
  short o = t - i;
  return o;
}

void testDchar()
{
    dchar d;
    uint i;
    /+
    static assert(!__traits(compiles, d = i));
    static assert(!__traits(compiles, d = i & 0x1fffff));
    +/
    d = i % 0x110000;
}

void bug1977_comment11()
{
    uint a;
    byte b = a & 1;
    // [this passes in 2.053.]
}

void bug1977_comment20()
{
    long a;
    int b = a % 1000;
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=9617

void test9617()
{
    void f1(int) {}
    void f2(short) {}
    void f3(byte) {}

    // Why these calls are accepted?
    static assert(!__traits(compiles, f1(ulong.max)));
    static assert(!__traits(compiles, f2(ulong.max)));
    static assert(!__traits(compiles, f3(ulong.max)));

    // But, if argument is not constant value, compilation fails.
    ulong x;
    static assert(!__traits(compiles, f1(x)));  // is not callable using argument types (ulong)
    static assert(!__traits(compiles, f2(x)));  // is not callable using argument types (ulong)
    static assert(!__traits(compiles, f3(x)));  // is not callable using argument types (ulong)

    void f4(uint) {}
    void f5(ushort) {}
    void f6(ubyte) {}

    // If parameter type is unsigned, it is collectly rejected
    static assert(!__traits(compiles, f4(ulong.max)));  // is not callable using argument types (ulong)
    static assert(!__traits(compiles, f5(ulong.max)));  // is not callable using argument types (ulong)
    static assert(!__traits(compiles, f6(ulong.max)));  // is not callable using argument types (ulong)
}

//import std.typetuple;
template TypeTuple(T...) { alias TypeTuple = T; }
template staticIota(size_t end)
{
    static if (0 < end)
        alias staticIota = TypeTuple!(staticIota!(end - 1), end - 1);
    else
        alias staticIota = TypeTuple!();
}
void test9617a()
{
    alias Repr = TypeTuple!(
        byte,   "127",      // T and literal representation of T.max
        ubyte,  "255",
        short,  "32767",
        ushort, "65535",
        int,    "2147483647",
        uint,   "4294967295",
        long,   "9223372036854775807",
        ulong,  "18446744073709551615"  // "" or "L" -> "signed integral overflow"
    );
    alias Indices = staticIota!(Repr.length / 2);

    foreach (t; Indices)
    {
        alias T = Repr[t * 2];
        void func(T)(T) {}
        alias func!T f;

        foreach (r; Indices)
        {
            alias S = Repr[r * 2];
            S src = S.max;

            enum x = Repr[r * 2 + 1];
            foreach (repr; TypeTuple!(S.stringof~".max", x~"", x~"U", x~"L", x~"LU"))
            {
                static if (S.sizeof != T.sizeof)
                static if (is(typeof(mixin(repr)) R))
                {
                    // "Compilable" test should be equal, even if
                    // the given argument is either constant or runtime variable.
                    enum ct = __traits(compiles, f( mixin(repr) ));
                    enum rt = __traits(compiles, f( src ));

                    static assert(ct == rt);
                    //import std.string;
                    //enum msg = format("%6s.max to %-6s variable/constant = %d/%d, constant_repr = (%s) %s",
                    //                    S.stringof, T.stringof, rt, ct, R.stringof, repr);
                    //static if (ct != rt) pragma(msg, msg);
                }
            }
        }
    }
}

void test10018(ubyte value)
{
    const int c = value;
    ubyte b = c;
    static assert(!__traits(compiles, b = c - 1));
    static assert(!__traits(compiles, b = c + 1));
    immutable int i = value;
    b = i;
    static assert(!__traits(compiles, b = i - 1));
    static assert(!__traits(compiles, b = i + 1));
}

void test13001(bool unknown)
{
    foreach (const i; 0..unknown?2:3)
    {
        ubyte b = i;
        static assert(!__traits(compiles, b = i - 1));
        b = i + 253;
        static assert(!__traits(compiles, b = i + 254));
    }
}

void test10310()
{
    int y;
    ubyte x = ((y & 252) ^ 2) + 1;
}

// https://issues.dlang.org/show_bug.cgi?id=15289
void test15289a()
{
    int [] arr = [1, 2, 3, 4];
    uint foo = 50 / arr.length;
}

void test15289b()
{
    int [] arr = [1, 2, 3, 4];
    uint foo = 50 % arr.length;
}

void testShiftRightOnNegative()
{
    int neg = -1;
    uint[] arr = [1, 2, 3];
    ubyte b;
    // Shift with negative value returns value in range [0, ulong.max]
    static assert(!__traits(compiles, b = arr.length >> neg));
    static assert(!__traits(compiles, b = arr.length << neg));
}
