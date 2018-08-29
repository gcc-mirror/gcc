/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -O2 -ffast-math -std=gnu11" } */

/* Test to make sure the compiler handles the standard _Float128 functions that
   have hardware support in ISA 3.0/power9.  */

#define __STDC_WANT_IEC_60559_TYPES_EXT__ 1

#ifndef __FP_FAST_FMAF128
#error "__FP_FAST_FMAF128 should be defined."
#endif

extern _Float128 copysignf128 (_Float128, _Float128);
extern _Float128 sqrtf128 (_Float128);
extern _Float128 fmaf128 (_Float128, _Float128, _Float128);
extern _Float128 ceilf128 (_Float128);
extern _Float128 floorf128 (_Float128);
extern _Float128 truncf128 (_Float128);
extern _Float128 roundf128 (_Float128);

_Float128
do_copysign (_Float128 a, _Float128 b)
{
  return copysignf128 (a, b);
}

_Float128
do_sqrt (_Float128 a)
{
  return sqrtf128 (a);
}

_Float128
do_fma (_Float128 a, _Float128 b, _Float128 c)
{
  return fmaf128 (a, b, c);
}

_Float128
do_fms (_Float128 a, _Float128 b, _Float128 c)
{
  return fmaf128 (a, b, -c);
}

_Float128
do_nfma (_Float128 a, _Float128 b, _Float128 c)
{
  return -fmaf128 (a, b, c);
}

_Float128
do_nfms (_Float128 a, _Float128 b, _Float128 c)
{
  return -fmaf128 (a, b, -c);
}

_Float128
do_ceil (_Float128 a)
{
  return ceilf128 (a);
}

_Float128
do_floor (_Float128 a)
{
  return floorf128 (a);
}

_Float128
do_trunc (_Float128 a)
{
  return truncf128 (a);
}

_Float128
do_round (_Float128 a)
{
  return roundf128 (a);
}

/* { dg-final { scan-assembler     {\mxscpsgnqp\M} } } */
/* { dg-final { scan-assembler     {\mxssqrtqp\M}  } } */
/* { dg-final { scan-assembler     {\mxsmaddqp\M}  } } */
/* { dg-final { scan-assembler     {\mxsmsubqp\M}  } } */
/* { dg-final { scan-assembler     {\mxsnmaddqp\M} } } */
/* { dg-final { scan-assembler     {\mxsnmsubqp\M} } } */
/* { dg-final { scan-assembler     {\mxsrqpi\M}    } } */
/* { dg-final { scan-assembler-not {\mbl\M}        } } */
