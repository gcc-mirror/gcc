/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mpower9-vector -O2 -ffast-math" } */

extern _Float128 copysignf128 (_Float128, _Float128);

/* Check copysign optimizations that are done for double are also done for
   _Float128.  */

_Float128
x_times_cs_one_negx (_Float128 x)
{
  return x * copysignf128 (1.0Q, -x);	/* XSNABSQP  */
}

_Float128
x_times_cs_one_x (_Float128 x)
{
  return x * copysignf128 (1.0Q, x);	/* XSABSQP  */
}

_Float128
cs_x_x (_Float128 x)
{
  return copysignf128 (x, x);		/* no operation.  */
}

/* { dg-final { scan-assembler-times {\mxsabsqp\M}  1 } } */
/* { dg-final { scan-assembler-times {\mxsnabsqp\M} 1 } } */
/* { dg-final { scan-assembler-not   {\mxscpsgnqp\M}  } } */
/* { dg-final { scan-assembler-not   {\mlxvx\M}       } } */
/* { dg-final { scan-assembler-not   {\mlxv\M}        } } */
/* { dg-final { scan-assembler-not   {\mbl\M}         } } */
