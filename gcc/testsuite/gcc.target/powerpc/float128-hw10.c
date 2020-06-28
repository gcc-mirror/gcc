/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-require-effective-target float128 } */
/* { dg-options "-mpower9-vector -O2" } */

extern _Float128 floorf128 (_Float128);
extern _Float128 ceilf128 (_Float128);
extern _Float128 roundf128 (_Float128);
extern _Float128 truncf128 (_Float128);

/* Check rounding optimizations that are done for double are also done for
   _Float128.  */

_Float128
floor_floor_x (_Float128 x)
{
  return floorf128 (floorf128 (x));
}

_Float128
ceil_ceil_x (_Float128 x)
{
  return ceilf128 (ceilf128 (x));
}

_Float128
trunc_trunc_x (_Float128 x)
{
  return truncf128 (truncf128 (x));
}

_Float128
round_round_x (_Float128 x)
{
  return roundf128 (roundf128 (x));
}

/* { dg-final { scan-assembler-times {\mxsrqpi\M} 4 } } */
/* { dg-final { scan-assembler-not   {\mbl\M}       } } */
