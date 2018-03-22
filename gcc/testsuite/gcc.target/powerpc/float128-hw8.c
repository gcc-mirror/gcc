/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -O2" } */

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
