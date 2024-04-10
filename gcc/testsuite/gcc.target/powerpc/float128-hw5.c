/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-require-effective-target float128 } */
/* { dg-options "-mvsx -O2 -ffast-math" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */

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
