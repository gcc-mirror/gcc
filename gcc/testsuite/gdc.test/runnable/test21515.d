// https://issues.dlang.org/show_bug.cgi?id=21515
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

extern(D) complex_float  dcomplexf() { return typeof(return)(2, 1); }
extern(D) complex_double dcomplex()  { return typeof(return)(2, 1); }
extern(D) complex_real   dcomplexl() { return typeof(return)(2, 1); }

extern(D) void dcomplexf(complex_float c) { assert(c.re == 2 && c.im == 1); }
extern(D) void dcomplex(complex_double c) { assert(c.re == 2 && c.im == 1); }
extern(D) void dcomplexl(complex_real c)  { assert(c.re == 2 && c.im == 1); }

extern(C) complex_float  ccomplexf() { return typeof(return)(2, 1); }
extern(C) complex_double ccomplex()  { return typeof(return)(2, 1); }
extern(C) complex_real   ccomplexl() { return typeof(return)(2, 1); }

extern(C) void ccomplexf2(complex_float c) { assert(c.re == 2 && c.im == 1); }
extern(C) void ccomplex2(complex_double c) { assert(c.re == 2 && c.im == 1); }
extern(C) void ccomplexl2(complex_real c)  { assert(c.re == 2 && c.im == 1); }

extern(C++) complex_float  cpcomplexf() { return typeof(return)(2, 1); }
extern(C++) complex_double cpcomplex()  { return typeof(return)(2, 1); }
extern(C++) complex_real   cpcomplexl() { return typeof(return)(2, 1); }

extern(C++) void cpcomplexf(complex_float c) { assert(c.re == 2 && c.im == 1); }
extern(C++) void cpcomplex(complex_double c) { assert(c.re == 2 && c.im == 1); }
extern(C++) void cpcomplexl(complex_real c)  { assert(c.re == 2 && c.im == 1); }

int main()
{
    auto a1 = dcomplexf();
    auto b1 = dcomplex();
    auto c1 = dcomplexl();
    assert(a1.re == 2 && a1.im == 1);
    assert(b1.re == 2 && b1.im == 1);
    assert(c1.re == 2 && c1.im == 1);
    dcomplexf(a1);
    dcomplex(b1);
    dcomplexl(c1);

    auto a2 = ccomplexf();
    auto b2 = ccomplex();
    auto c2 = ccomplexl();
    assert(a2.re == 2 && a2.im == 1);
    assert(b2.re == 2 && b2.im == 1);
    assert(c2.re == 2 && c2.im == 1);
    ccomplexf2(a2);
    ccomplex2(b2);
    ccomplexl2(c2);

    auto a3 = cpcomplexf();
    auto b3 = cpcomplex();
    auto c3 = cpcomplexl();
    assert(a3.re == 2 && a3.im == 1);
    assert(b3.re == 2 && b3.im == 1);
    assert(c3.re == 2 && c3.im == 1);
    cpcomplexf(a3);
    cpcomplex(b3);
    cpcomplexl(c3);

    return 0;
}
