// https://issues.dlang.org/show_bug.cgi?id=21515
// EXTRA_CPP_SOURCES: test21515.cpp
// CXXFLAGS: -std=c++11
// DISABLED: win32 win64

// ABI layout of native complex
struct _Complex(T) { T re; T im; }

// Special enum definitions.
version (Posix)
{
    align(float.alignof)  enum __c_complex_float : _Complex!float;
    align(double.alignof) enum __c_complex_double : _Complex!double;
    align(real.alignof)   enum __c_complex_real : _Complex!real;
}
else
{
    align(float.sizeof * 2)  enum __c_complex_float : _Complex!float;
    align(double.sizeof * 2) enum __c_complex_double : _Complex!double;
    align(real.alignof)      enum __c_complex_real : _Complex!real;
}
alias complex_float = __c_complex_float;
alias complex_double = __c_complex_double;
alias complex_real = __c_complex_real;

extern(C) complex_float  ccomplexf();
extern(C) complex_double ccomplex();
extern(C) complex_real   ccomplexl();
extern(C) void           ccomplexf2(complex_float c);
extern(C) void           ccomplex2(complex_double c);
extern(C) void           ccomplexl2(complex_real c);

extern(C++) complex_float  cpcomplexf();
extern(C++) complex_double cpcomplex();
extern(C++) complex_real   cpcomplexl();
extern(C++) void           cpcomplexf(complex_float c);
extern(C++) void           cpcomplex(complex_double c);
extern(C++) void           cpcomplexl(complex_real c);

struct wrap_complexf { complex_float c; alias c this; };
struct wrap_complex  { complex_double c; alias c this; };
struct wrap_complexl { complex_real c; alias c this; };

extern(C++) wrap_complexf wcomplexf();
extern(C++) wrap_complex  wcomplex();
extern(C++) wrap_complexl wcomplexl();
extern(C++) void          wcomplexf(wrap_complexf c);
extern(C++) void          wcomplex(wrap_complex c);
extern(C++) void          wcomplexl(wrap_complexl c);

struct soft_complexf { float re; float im; };
struct soft_complex  { double re; double im; };
struct soft_complexl { real re; real im; };

extern(C++) soft_complexf scomplexf();
extern(C++) soft_complex  scomplex();
extern(C++) soft_complexl scomplexl();
extern(C++) void          scomplexf(soft_complexf c);
extern(C++) void          scomplex(soft_complex c);
extern(C++) void          scomplexl(soft_complexl c);

int main()
{
    auto a1 = ccomplexf();
    auto b1 = ccomplex();
    auto c1 = ccomplexl();
    assert(a1.re == 2 && a1.im == 1);
    assert(b1.re == 2 && b1.im == 1);
    assert(c1.re == 2 && c1.im == 1);
    ccomplexf2(a1);
    ccomplex2(b1);
    ccomplexl2(c1);

    auto a2 = cpcomplexf();
    auto b2 = cpcomplex();
    auto c2 = cpcomplexl();
    assert(a2.re == 2 && a2.im == 1);
    assert(b2.re == 2 && b2.im == 1);
    assert(c2.re == 2 && c2.im == 1);
    cpcomplexf(a2);
    cpcomplex(b2);
    cpcomplexl(c2);

    auto a3 = wcomplexf();
    auto b3 = wcomplex();
    auto c3 = wcomplexl();
    assert(a3.re == 2 && a3.im == 1);
    assert(b3.re == 2 && b3.im == 1);
    assert(c3.re == 2 && c3.im == 1);
    wcomplexf(a3);
    wcomplex(b3);
    wcomplexl(c3);

    auto a4 = scomplexf();
    auto b4 = scomplex();
    auto c4 = scomplexl();
    assert(a4.re == 2 && a4.im == 1);
    assert(b4.re == 2 && b4.im == 1);
    assert(c4.re == 2 && c4.im == 1);
    scomplexf(a4);
    scomplex(b4);
    scomplexl(c4);

    return 0;
}
