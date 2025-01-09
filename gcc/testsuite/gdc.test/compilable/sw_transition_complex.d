// PERMUTE_ARGS:
// REQUIRED_ARGS: -verrors=simple -unittest -verrors=0

/*
TEST_OUTPUT:
---
compilable/sw_transition_complex.d(15): Deprecation: use of complex type `creal` is deprecated, use `std.complex.Complex!(real)` instead
compilable/sw_transition_complex.d(16): Deprecation: use of complex type `cdouble` is deprecated, use `std.complex.Complex!(double)` instead
compilable/sw_transition_complex.d(17): Deprecation: use of complex type `cfloat` is deprecated, use `std.complex.Complex!(float)` instead
compilable/sw_transition_complex.d(19): Deprecation: use of imaginary type `ireal` is deprecated, use `real` instead
compilable/sw_transition_complex.d(20): Deprecation: use of imaginary type `idouble` is deprecated, use `double` instead
compilable/sw_transition_complex.d(21): Deprecation: use of imaginary type `ifloat` is deprecated, use `float` instead
---
*/
creal c80value;
cdouble c64value;
cfloat c32value;

ireal i80value;
idouble i64value;
ifloat i32value;

/*
TEST_OUTPUT:
---
compilable/sw_transition_complex.d(34): Deprecation: use of complex type `creal*` is deprecated, use `std.complex.Complex!(real)` instead
compilable/sw_transition_complex.d(35): Deprecation: use of complex type `cdouble*` is deprecated, use `std.complex.Complex!(double)` instead
compilable/sw_transition_complex.d(36): Deprecation: use of complex type `cfloat*` is deprecated, use `std.complex.Complex!(float)` instead
compilable/sw_transition_complex.d(38): Deprecation: use of imaginary type `ireal*` is deprecated, use `real` instead
compilable/sw_transition_complex.d(39): Deprecation: use of imaginary type `idouble*` is deprecated, use `double` instead
compilable/sw_transition_complex.d(40): Deprecation: use of imaginary type `ifloat*` is deprecated, use `float` instead
---
*/
creal* c80pointer;
cdouble* c64pointer;
cfloat* c32pointer;

ireal* i80pointer;
idouble* i64pointer;
ifloat* i32pointer;

/*
TEST_OUTPUT:
---
compilable/sw_transition_complex.d(53): Deprecation: use of complex type `creal[]*` is deprecated, use `std.complex.Complex!(real)` instead
compilable/sw_transition_complex.d(54): Deprecation: use of complex type `cdouble[]*` is deprecated, use `std.complex.Complex!(double)` instead
compilable/sw_transition_complex.d(55): Deprecation: use of complex type `cfloat[]*` is deprecated, use `std.complex.Complex!(float)` instead
compilable/sw_transition_complex.d(57): Deprecation: use of imaginary type `ireal[]*` is deprecated, use `real` instead
compilable/sw_transition_complex.d(58): Deprecation: use of imaginary type `idouble[]*` is deprecated, use `double` instead
compilable/sw_transition_complex.d(59): Deprecation: use of imaginary type `ifloat[]*` is deprecated, use `float` instead
---
*/
creal[]* c80arrayp;
cdouble[]* d64arrayp;
cfloat[]* c32arrayp;

ireal[]* i80arrayp;
idouble[]* i64arrayp;
ifloat[]* i32arrayp;

/*
TEST_OUTPUT:
---
compilable/sw_transition_complex.d(72): Deprecation: use of complex type `creal[4][]*` is deprecated, use `std.complex.Complex!(real)` instead
compilable/sw_transition_complex.d(73): Deprecation: use of complex type `cdouble[4][]*` is deprecated, use `std.complex.Complex!(double)` instead
compilable/sw_transition_complex.d(74): Deprecation: use of complex type `cfloat[4][]*` is deprecated, use `std.complex.Complex!(float)` instead
compilable/sw_transition_complex.d(76): Deprecation: use of imaginary type `ireal[4][]*` is deprecated, use `real` instead
compilable/sw_transition_complex.d(77): Deprecation: use of imaginary type `idouble[4][]*` is deprecated, use `double` instead
compilable/sw_transition_complex.d(78): Deprecation: use of imaginary type `ifloat[4][]*` is deprecated, use `float` instead
---
*/
creal[4][]* c80sarrayp;
cdouble[4][]* c64sarrayp;
cfloat[4][]* c32sarrayp;

ireal[4][]* i80sarrayp;
idouble[4][]* i64sarrayp;
ifloat[4][]* i32sarrayp;

/*
TEST_OUTPUT:
---
compilable/sw_transition_complex.d(96): Deprecation: use of complex type `creal` is deprecated, use `std.complex.Complex!(real)` instead
compilable/sw_transition_complex.d(97): Deprecation: use of complex type `creal*` is deprecated, use `std.complex.Complex!(real)` instead
compilable/sw_transition_complex.d(98): Deprecation: use of complex type `creal[]` is deprecated, use `std.complex.Complex!(real)` instead
compilable/sw_transition_complex.d(99): Deprecation: use of complex type `creal[4]` is deprecated, use `std.complex.Complex!(real)` instead
compilable/sw_transition_complex.d(101): Deprecation: use of imaginary type `ireal` is deprecated, use `real` instead
compilable/sw_transition_complex.d(102): Deprecation: use of imaginary type `ireal*` is deprecated, use `real` instead
compilable/sw_transition_complex.d(103): Deprecation: use of imaginary type `ireal[]` is deprecated, use `real` instead
compilable/sw_transition_complex.d(104): Deprecation: use of imaginary type `ireal[4]` is deprecated, use `real` instead
---
*/
alias C14488 = creal;
alias I14488 = ireal;

C14488 calias1;
C14488* calias2;
C14488[] calias3;
C14488[4] calias4;

I14488 ialias1;
I14488* ialias2;
I14488[] ialias3;
I14488[4] ialias4;

/*
TEST_OUTPUT:
---
compilable/sw_transition_complex.d(115): Deprecation: use of complex type `cdouble` is deprecated, use `std.complex.Complex!(double)` instead
compilable/sw_transition_complex.d(116): Deprecation: use of imaginary type `idouble` is deprecated, use `double` instead
compilable/sw_transition_complex.d(117): Deprecation: use of complex type `cdouble` is deprecated, use `std.complex.Complex!(double)` instead
compilable/sw_transition_complex.d(118): Deprecation: use of complex type `cdouble[]` is deprecated, use `std.complex.Complex!(double)` instead
---
*/
auto cauto = 1 + 0i;
auto iauto = 1i;
size_t c64sizeof = (cdouble).sizeof;
TypeInfo c64ti = typeid(cdouble[]);

/*
TEST_OUTPUT:
---
compilable/sw_transition_complex.d(128): Deprecation: use of complex type `creal*` is deprecated, use `std.complex.Complex!(real)` instead
compilable/sw_transition_complex.d(128): Deprecation: use of imaginary type `ireal` is deprecated, use `real` instead
compilable/sw_transition_complex.d(132): Deprecation: use of complex type `creal` is deprecated, use `std.complex.Complex!(real)` instead
---
*/
void test14488a(creal *p, real r, ireal i)
{
}

creal test14488b()
{
    return 1 + 0i;
}

// Forward referenced types shouldn't cause errors during test for complex or imaginary.
enum E;
struct S;

void test14488c(E *e, S *s)
{
}

// https://issues.dlang.org/show_bug.cgi?id=18212
// Usage of cfloat,cdouble,cfloat,ifloat,idouble,ireal shouldn't trigger an error in deprecated code
deprecated void test18212(creal c){}
deprecated unittest
{
    ireal a = 2i;
    creal b = 2 + 3i;
}
deprecated struct Foo
{
    ifloat a = 2i;
    cfloat b = 2f + 2i;
}

// https://issues.dlang.org/show_bug.cgi?id=18218
static assert(__traits(isDeprecated, cfloat));
static assert(__traits(isDeprecated, cdouble));
static assert(__traits(isDeprecated, creal));
static assert(__traits(isDeprecated, ifloat));
static assert(__traits(isDeprecated, idouble));
static assert(__traits(isDeprecated, ireal));
static assert(!__traits(isDeprecated, float));
static assert(!__traits(isDeprecated, double));
static assert(!__traits(isDeprecated, real));
static assert(!__traits(isDeprecated, int));
static assert(!__traits(isDeprecated, long));
static assert(!__traits(isDeprecated, ubyte));
static assert(!__traits(isDeprecated, char));
static assert(!__traits(isDeprecated, bool));
static assert(!__traits(isDeprecated, S));
