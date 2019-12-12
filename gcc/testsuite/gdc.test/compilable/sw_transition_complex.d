// PERMUTE_ARGS:
// REQUIRED_ARGS: -c -transition=complex

/*
TEST_OUTPUT:
---
compilable/sw_transition_complex.d(15): use of complex type 'creal' is scheduled for deprecation, use 'std.complex.Complex!(real)' instead
compilable/sw_transition_complex.d(16): use of complex type 'cdouble' is scheduled for deprecation, use 'std.complex.Complex!(double)' instead
compilable/sw_transition_complex.d(17): use of complex type 'cfloat' is scheduled for deprecation, use 'std.complex.Complex!(float)' instead
compilable/sw_transition_complex.d(19): use of imaginary type 'ireal' is scheduled for deprecation, use 'real' instead
compilable/sw_transition_complex.d(20): use of imaginary type 'idouble' is scheduled for deprecation, use 'double' instead
compilable/sw_transition_complex.d(21): use of imaginary type 'ifloat' is scheduled for deprecation, use 'float' instead
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
compilable/sw_transition_complex.d(34): use of complex type 'creal*' is scheduled for deprecation, use 'std.complex.Complex!(real)' instead
compilable/sw_transition_complex.d(35): use of complex type 'cdouble*' is scheduled for deprecation, use 'std.complex.Complex!(double)' instead
compilable/sw_transition_complex.d(36): use of complex type 'cfloat*' is scheduled for deprecation, use 'std.complex.Complex!(float)' instead
compilable/sw_transition_complex.d(38): use of imaginary type 'ireal*' is scheduled for deprecation, use 'real' instead
compilable/sw_transition_complex.d(39): use of imaginary type 'idouble*' is scheduled for deprecation, use 'double' instead
compilable/sw_transition_complex.d(40): use of imaginary type 'ifloat*' is scheduled for deprecation, use 'float' instead
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
compilable/sw_transition_complex.d(53): use of complex type 'creal[]*' is scheduled for deprecation, use 'std.complex.Complex!(real)' instead
compilable/sw_transition_complex.d(54): use of complex type 'cdouble[]*' is scheduled for deprecation, use 'std.complex.Complex!(double)' instead
compilable/sw_transition_complex.d(55): use of complex type 'cfloat[]*' is scheduled for deprecation, use 'std.complex.Complex!(float)' instead
compilable/sw_transition_complex.d(57): use of imaginary type 'ireal[]*' is scheduled for deprecation, use 'real' instead
compilable/sw_transition_complex.d(58): use of imaginary type 'idouble[]*' is scheduled for deprecation, use 'double' instead
compilable/sw_transition_complex.d(59): use of imaginary type 'ifloat[]*' is scheduled for deprecation, use 'float' instead
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
compilable/sw_transition_complex.d(72): use of complex type 'creal[4][]*' is scheduled for deprecation, use 'std.complex.Complex!(real)' instead
compilable/sw_transition_complex.d(73): use of complex type 'cdouble[4][]*' is scheduled for deprecation, use 'std.complex.Complex!(double)' instead
compilable/sw_transition_complex.d(74): use of complex type 'cfloat[4][]*' is scheduled for deprecation, use 'std.complex.Complex!(float)' instead
compilable/sw_transition_complex.d(76): use of imaginary type 'ireal[4][]*' is scheduled for deprecation, use 'real' instead
compilable/sw_transition_complex.d(77): use of imaginary type 'idouble[4][]*' is scheduled for deprecation, use 'double' instead
compilable/sw_transition_complex.d(78): use of imaginary type 'ifloat[4][]*' is scheduled for deprecation, use 'float' instead
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
compilable/sw_transition_complex.d(96): use of complex type 'creal' is scheduled for deprecation, use 'std.complex.Complex!(real)' instead
compilable/sw_transition_complex.d(97): use of complex type 'creal*' is scheduled for deprecation, use 'std.complex.Complex!(real)' instead
compilable/sw_transition_complex.d(98): use of complex type 'creal[]' is scheduled for deprecation, use 'std.complex.Complex!(real)' instead
compilable/sw_transition_complex.d(99): use of complex type 'creal[4]' is scheduled for deprecation, use 'std.complex.Complex!(real)' instead
compilable/sw_transition_complex.d(101): use of imaginary type 'ireal' is scheduled for deprecation, use 'real' instead
compilable/sw_transition_complex.d(102): use of imaginary type 'ireal*' is scheduled for deprecation, use 'real' instead
compilable/sw_transition_complex.d(103): use of imaginary type 'ireal[]' is scheduled for deprecation, use 'real' instead
compilable/sw_transition_complex.d(104): use of imaginary type 'ireal[4]' is scheduled for deprecation, use 'real' instead
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
compilable/sw_transition_complex.d(115): use of complex type 'cdouble' is scheduled for deprecation, use 'std.complex.Complex!(double)' instead
compilable/sw_transition_complex.d(116): use of imaginary type 'idouble' is scheduled for deprecation, use 'double' instead
compilable/sw_transition_complex.d(117): use of complex type 'cdouble' is scheduled for deprecation, use 'std.complex.Complex!(double)' instead
compilable/sw_transition_complex.d(118): use of complex type 'cdouble[]' is scheduled for deprecation, use 'std.complex.Complex!(double)' instead
---
*/
auto cauto = 1 + 0i;
auto iauto = 1i;
size_t c64sizeof = (cdouble).sizeof;
TypeInfo c64ti = typeid(cdouble[]);

/*
TEST_OUTPUT:
---
compilable/sw_transition_complex.d(128): use of complex type 'creal*' is scheduled for deprecation, use 'std.complex.Complex!(real)' instead
compilable/sw_transition_complex.d(128): use of imaginary type 'ireal' is scheduled for deprecation, use 'real' instead
compilable/sw_transition_complex.d(132): use of complex type 'creal' is scheduled for deprecation, use 'std.complex.Complex!(real)' instead
---
*/
void test14488a(creal *p, real r, ireal i)
{
}

creal test14488b()
{
    return 1 + 0i;
}

