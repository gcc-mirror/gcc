/* { dg-do compile } */
/* { dg-require-effective-target ppc_float128_hw } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

#ifndef TYPE
#ifdef __LONG_DOUBLE_IEEE128__
#define TYPE long double

#else
#define TYPE _Float128
#endif
#endif

/* Verify that the ISA 3.1 (power10) IEEE 128-bit conditional move instructions
   are generated.  */

TYPE
eq (TYPE a, TYPE b, TYPE c, TYPE d)
{
  return (a == b) ? c : d;
}

TYPE
ne (TYPE a, TYPE b, TYPE c, TYPE d)
{
  return (a != b) ? c : d;
}

TYPE
lt (TYPE a, TYPE b, TYPE c, TYPE d)
{
  return (a < b) ? c : d;
}

TYPE
le (TYPE a, TYPE b, TYPE c, TYPE d)
{
  return (a <= b) ? c : d;
}

TYPE
gt (TYPE a, TYPE b, TYPE c, TYPE d)
{
  return (a > b) ? c : d;
}

TYPE
ge (TYPE a, TYPE b, TYPE c, TYPE d)
{
  return (a >= b) ? c : d;
}

/* { dg-final { scan-assembler-times {\mxscmpeqqp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxscmpgeqp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxscmpgtqp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxsel\M}     6 } } */
/* { dg-final { scan-assembler-not   {\mxscmpuqp\M}    } } */
