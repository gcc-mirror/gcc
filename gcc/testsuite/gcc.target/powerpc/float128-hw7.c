/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -O2" } */

extern _Float128 fabsf128 (_Float128);
extern _Float128 copysignf128 (_Float128, _Float128);

/* Check copysign optimizations that are done for double are also done for
   _Float128.  */

_Float128
cs_x_pos1 (_Float128 x)
{
  return copysignf128 (x, 1.0Q);		/* XSABSQP.  */
}

_Float128 cs_x_neg2 (_Float128 x)
{
  return copysignf128 (x, -2.0Q);		/* XSNABSQP.  */
}

/* { dg-final { scan-assembler-times {\mxsabsqp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mxsnabsqp\M} 1 } } */
/* { dg-final { scan-assembler-not   {\mxscpsgnqp\M}  } } */
/* { dg-final { scan-assembler-not   {\mlxvx\M}       } } */
/* { dg-final { scan-assembler-not   {\mlxv\M}        } } */
/* { dg-final { scan-assembler-not   {\mbl\M}         } } */
