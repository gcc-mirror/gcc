// EXTRA_CPP_SOURCES: cpp_abi_tests.cpp
// CXXFLAGS(linux freebsd osx netbsd dragonflybsd): -std=c++11

// N.B MSVC doesn't have a C++11 switch, but it defaults to the latest fully-supported standard
// N.B MSVC 2013 doesn't support char16_t/char32_t

version(Posix)
    enum __c_wchar_t : dchar;
else version(Windows)
    enum __c_wchar_t : wchar;
alias wchar_t = __c_wchar_t;

extern(C++) {

struct S
{
    float a = 1;
}

struct S18784
{
    int i;
    this(int);
}

extern(C++, std)
{
    struct test19248_ {int a = 42;}
}
extern(C++, `std`)
{
    struct test19248 {int a = 34;}
}

struct Sdtor
{
    extern __gshared int counter;
    ~this();
}
void consume(Sdtor);
void consume2(Sdtor value){}
void doConsume2(ref Sdtor);

struct SPack(Args...)
{
    int i;
}
alias SInt = SPack!int;

bool   passthrough(bool   value);
byte   passthrough(byte   value);
ubyte  passthrough(ubyte  value);
char   passthrough(char   value);
wchar  passthrough(wchar  value);
dchar  passthrough(dchar  value);
wchar_t passthrough(wchar_t value);
short  passthrough(short  value);
ushort passthrough(ushort value);
int    passthrough(int    value);
uint   passthrough(uint   value);
long   passthrough(long   value);
ulong  passthrough(ulong  value);
float  passthrough(float  value);
double passthrough(double value);
S      passthrough(S      value);
test19248 passthrough(const(test19248) value);
std.test19248_ passthrough(const(std.test19248_) value);
SInt   passthrough(SInt   value);

bool   passthrough_ptr(bool   *value);
byte   passthrough_ptr(byte   *value);
ubyte  passthrough_ptr(ubyte  *value);
char   passthrough_ptr(char   *value);
wchar  passthrough_ptr(wchar  *value);
dchar  passthrough_ptr(dchar  *value);
wchar_t passthrough_ptr(wchar_t *value);
short  passthrough_ptr(short  *value);
ushort passthrough_ptr(ushort *value);
int    passthrough_ptr(int    *value);
uint   passthrough_ptr(uint   *value);
long   passthrough_ptr(long   *value);
ulong  passthrough_ptr(ulong  *value);
float  passthrough_ptr(float  *value);
double passthrough_ptr(double *value);
S      passthrough_ptr(S      *value);
test19248 passthrough_ptr(const(test19248)* value);
std.test19248_ passthrough_ptr(const(std.test19248_)* value);
SInt   passthrough_ptr(SInt   *value);

bool   passthrough_ref(ref bool   value);
byte   passthrough_ref(ref byte   value);
ubyte  passthrough_ref(ref ubyte  value);
char   passthrough_ref(ref char   value);
wchar  passthrough_ref(ref wchar  value);
dchar  passthrough_ref(ref dchar  value);
wchar_t passthrough_ref(ref wchar_t value);
short  passthrough_ref(ref short  value);
ushort passthrough_ref(ref ushort value);
int    passthrough_ref(ref int    value);
uint   passthrough_ref(ref uint   value);
long   passthrough_ref(ref long   value);
ulong  passthrough_ref(ref ulong  value);
float  passthrough_ref(ref float  value);
double passthrough_ref(ref double value);
S      passthrough_ref(ref S      value);
test19248 passthrough_ref(ref const(test19248) value);
std.test19248_ passthrough_ref(ref const(std.test19248_) value);
SInt   passthrough_ref(ref SInt   value);
}

template IsSigned(T)
{
    enum IsSigned = is(T==byte)  ||
                    is(T==short) ||
                    is(T==int)   ||
                    is(T==long);
}

template IsUnsigned(T)
{
    enum IsUnsigned = is(T==ubyte)  ||
                      is(T==ushort) ||
                      is(T==uint)   ||
                      is(T==ulong);
}

template IsIntegral(T)
{
    enum IsIntegral = IsSigned!T || IsUnsigned!T;
}

template IsFloatingPoint(T)
{
    enum IsFloatingPoint = is(T==float) || is(T==double) || is(T==real);
}

template IsBoolean(T)
{
    enum IsBoolean = is(T==bool);
}

template IsSomeChar(T)
{
    enum IsSomeChar = is(T==char) || is(T==wchar) || is(T==dchar) || is(T==wchar_t);
}

void check(T)(T actual, T expected)
{
    assert(actual is expected);
}

void check(T)(T value)
{
    check(passthrough(value), value);
    check(passthrough_ptr(&value), value);
    check(passthrough_ref(value), value);
}

T[] values(T)()
{
    T[] values;
    static if(IsBoolean!T)
    {
        values ~= true;
        values ~= false;
    }
    else static if(IsSomeChar!T)
    {
        values ~= T.init;
        values ~= T('a');
        values ~= T('z');
    }
    else
    {
        values ~= T(0);
        values ~= T(1);
        static if(IsIntegral!T)
        {
            static if(IsSigned!T) values ~= T.min;
            values ~= T.max;
        }
        else static if(IsFloatingPoint!T)
        {
            values ~= T.nan;
            values ~= T.min_normal;
            values ~= T.max;
        }
        else
        {
            assert(0);
        }
    }
    return values;
}

extern(C++, `ns1`)
 {
    // C++: `const char*, const char**`
    int constFunction1(const(char)*, const(char)**);
    // C++: `const char*, const char* const*`
    int constFunction2(const(char)*, const(char*)*);
    // C++: `const char* const, const char* const* const*`
    int constFunction3(const(char*), const(char**)*);
    // C++: `const char* const, const char* const* const* const`
    int constFunction4(const(char*), const(char***));
}

extern(C++)
{
    // https://issues.dlang.org/show_bug.cgi?id=19563

    struct SmallStruct
    {
        int i;
        this(int i) { this.i = i; }
        this(ref const SmallStruct); // implemented in C++
    }
    void smallStructTest(SmallStruct p);
    void smallStructCallBack(SmallStruct p)
    {
        assert(p.i == 62);
    }
}

/*********************************************/
// https://issues.dlang.org/show_bug.cgi?id=23195

extern (C++)
{
    struct FF
    {
        float x, y;

        ~this() { }
    }

    float draw(FF min, FF max);

    void test23195()
    {
        FF a = { 1, 2 };
        FF b = { 3, 4 };
        float f = draw(a, b);
        assert(f == 1234);
    }

    /*********************/

    struct FF2
    {
        float x, y;

        this(int i) { }
    }

    float draw2(FF2 min, FF2 max);

    void test23195_2()
    {
        FF2 a; a.x = 1; a.y = 2;
        FF2 b; b.x = 3; b.y = 4;
        float f = draw2(a, b);
        assert(f == 1234);
    }
}

/*********************************************/

void main()
{
    foreach(bool val; values!bool())     check(val);
    foreach(byte val; values!byte())     check(val);
    foreach(ubyte val; values!ubyte())   check(val);
    foreach(char val; values!char())     check(val);
version(CppRuntime_DigitalMars){} else
version(CppRuntime_Microsoft)
{
// TODO: figure out how to detect VS2013 which doesn't support char16_t/char32_t
}
else
{
    foreach(wchar val; values!wchar())   check(val);
    foreach(dchar val; values!dchar())   check(val);
}
    foreach(wchar_t val; values!wchar_t()) check(val);
    foreach(short val; values!short())   check(val);
    foreach(ushort val; values!ushort()) check(val);
    foreach(int val; values!int())       check(val);
    foreach(uint val; values!uint())     check(val);
    foreach(long val; values!long())     check(val);
    foreach(ulong val; values!ulong())   check(val);
    foreach(float val; values!float())   check(val);
    foreach(double val; values!double()) check(val);
    check(S());
    check(test19248());
    check(std.test19248_());
    check(SInt());

    assert(constFunction1(null, null) == 1);
    assert(constFunction2(null, null) == 2);
    assert(constFunction3(null, null) == 3);
    assert(constFunction4(null, null) == 42);

    auto ss = SmallStruct(42);
    smallStructTest(ss);
    assert(ss.i == 42);
    assert(S18784(1).i == 1);

    {
        Sdtor sd;
        assert(Sdtor.counter == 0);
        consume(sd);
        assert(Sdtor.counter == 1);
        doConsume2(sd);
        assert(Sdtor.counter == 2);
    }
    test23195();
    test23195_2();
}
