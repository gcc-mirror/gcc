#include <assert.h>
#include <complex.h>

// Use custom types for inspecting parts instead of including tgmath.h
union cfloat_t { _Complex float z; struct { float re; float im; }; };
union cdouble_t { _Complex double z; struct { double re; double im; }; };
union creal_t { _Complex long double z; struct { long double re; long double im; }; };

// extern(C) tests
extern "C" _Complex float ccomplexf() { return 2.0f+I; }
extern "C" _Complex double ccomplex() { return 2.0+I; }
extern "C" _Complex long double ccomplexl() { return 2.0L+I; }
extern "C" void ccomplexf2(_Complex float c) { cfloat_t z = {c}; assert(z.re == 2 && z.im == 1); }
extern "C" void ccomplex2(_Complex double c) { cdouble_t z = {c}; assert(z.re == 2 && z.im == 1); }
extern "C" void ccomplexl2(_Complex long double c) { creal_t z = {c}; assert(z.re == 2 && z.im == 1); }

// extern(C++) tests
_Complex float cpcomplexf() { return 2.0f+I; }
_Complex double cpcomplex() { return 2.0+I; }
_Complex long double cpcomplexl() { return 2.0L+I; }
void cpcomplexf(_Complex float c) { cfloat_t z = {c}; assert(z.re == 2 && z.im == 1); }
void cpcomplex(_Complex double c) { cdouble_t z = {c}; assert(z.re == 2 && z.im == 1); }
void cpcomplexl(_Complex long double c) { creal_t z = {c}; assert(z.re == 2 && z.im == 1); }

// Struct tests
struct wrap_complexf { _Complex float c; };
struct wrap_complex { _Complex double c; };
struct wrap_complexl { _Complex long double c; };

wrap_complexf wcomplexf()
{
    wrap_complexf s;
    s.c = 2.0f+I;
    return s;
}

wrap_complex wcomplex()
{
    wrap_complex s;
    s.c = 2.0+I;
    return s;
}

wrap_complexl wcomplexl()
{
    wrap_complexl s;
    s.c = 2.0L+I;
    return s;
}

void wcomplexf(wrap_complexf s) { cfloat_t z = {s.c}; assert(z.re == 2 && z.im == 1); }
void wcomplex(wrap_complex s)   { cdouble_t z = {s.c}; assert(z.re == 2 && z.im == 1); }
void wcomplexl(wrap_complexl s) { creal_t z = {s.c}; assert(z.re == 2 && z.im == 1); }

struct soft_complexf { float re; float im; };
struct soft_complex { double re; double im; };
struct soft_complexl { long double re; long double im; };

soft_complexf scomplexf()
{
    soft_complexf s;
    s.re = 2.0f; s.im = 1.0f;
    return s;
}

soft_complex scomplex()
{
    soft_complex s;
    s.re = 2.0; s.im = 1.0;
    return s;
}

soft_complexl scomplexl()
{
    soft_complexl s;
    s.re = 2.0L; s.im = 1.0L;
    return s;
}

void scomplexf(soft_complexf s) { assert(s.re == 2 && s.im == 1); }
void scomplex(soft_complex s)   { assert(s.re == 2 && s.im == 1); }
void scomplexl(soft_complexl s) { assert(s.re == 2 && s.im == 1); }
