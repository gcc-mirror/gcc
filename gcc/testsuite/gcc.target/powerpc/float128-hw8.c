/* { dg-do compile { target lp64 } } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target float128 } */

extern _Float128 fminf128 (_Float128, _Float128);
extern _Float128 fmaxf128 (_Float128, _Float128);

/* Check min/max optimizations that are done for double are also done for
   _Float128.  */

_Float128
min_x_x (_Float128 x)
{
  return fminf128 (x, x);
}

_Float128
max_x_x (_Float128 x)
{
  return fmaxf128 (x, x);
}

/* { dg-final { scan-assembler-not {\mxscmpuqp\M} } } */
/* { dg-final { scan-assembler-not {\mbl\M}       } } */
