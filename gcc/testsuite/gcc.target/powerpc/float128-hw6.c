/* { dg-do compile { target lp64 } } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target float128 } */

extern _Float128 fabsf128 (_Float128);
extern _Float128 copysignf128 (_Float128, _Float128);

/* Check copysign optimizations that are done for double are also done for
   _Float128.  */

_Float128
cs_negx_y (_Float128 x, _Float128 y)
{
  return copysignf128 (-x, y);			/* eliminate negation.  */
}

_Float128
cs_absx_y (_Float128 x, _Float128 y)
{
  return copysignf128 (fabsf128 (x), y);	/* eliminate fabsf128.  */
}

/* { dg-final { scan-assembler-times {\mxscpsgnqp\M} 2 } } */
/* { dg-final { scan-assembler-not   {\mxsnegqp\M}     } } */
/* { dg-final { scan-assembler-not   {\mxsabsqp\M}     } } */
/* { dg-final { scan-assembler-not   {\mbl\M}          } } */
