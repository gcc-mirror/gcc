/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-options "-mvsx -O2 -mfloat128" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */
/* { dg-require-effective-target powerpc_vsx } */

/* PR 81959, the compiler raised on unrecognizable insn message in converting
   int to __float128, where the int had a PRE_INC in the address.  */

#ifndef ARRAY_SIZE
#define ARRAY_SIZE 1024
#endif

void
convert_int_to_float128 (__float128 * __restrict__ p,
			 int * __restrict__ q)
{
  unsigned long i;

  for (i = 0; i < ARRAY_SIZE; i++)
    p[i] = (__float128)q[i];
}

/* { dg-final { scan-assembler     {\mlfiwax\M|\mlxsiwax\M} } } */
/* { dg-final { scan-assembler     {\mxscvsdqp\M}           } } */
/* { dg-final { scan-assembler-not {\mmtvsrd\M}             } } */
/* { dg-final { scan-assembler-not {\mmtvsrw[sz]\M}         } } */
