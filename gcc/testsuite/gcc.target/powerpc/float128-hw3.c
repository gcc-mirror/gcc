/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-require-effective-target float128 } */
/* { dg-options "-mvsx -O2 -ffast-math -std=c11 -mno-pcrel" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */

/* Test to make sure the compiler calls the external function instead of doing
   the built-in processing for _Float128 functions that have hardware support
   in ISA 3.0/power9 if are in strict standards mode, where the <func>f128 name
   is not a synonym for __builtin_<func>f128.  */

extern _Float128 copysignf128 (_Float128, _Float128);
extern _Float128 sqrtf128 (_Float128);
extern _Float128 fmaf128 (_Float128, _Float128, _Float128);

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

/* { dg-final { scan-assembler-not   {\mxscpsgnqp\M} } } */
/* { dg-final { scan-assembler-not   {\mxssqrtqp\M}  } } */
/* { dg-final { scan-assembler-not   {\mxsmaddqp\M}  } } */
/* { dg-final { scan-assembler-not   {\mxsmsubqp\M}  } } */
/* { dg-final { scan-assembler-not   {\mxsnmaddqp\M} } } */
/* { dg-final { scan-assembler-not   {\mxsnmsubqp\M} } } */
/* { dg-final { scan-assembler-times {\mbl\M} 6      } } */
