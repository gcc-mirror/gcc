/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

/* Test that small VLS types are passed in GPRs even when filling
   the argument registers.  */

typedef int __attribute__((vector_size(8))) v2si;

v2si test_vls_multiple_gprs (int a0, int a1, int a2, int a3,
			     int a4, int a5, v2si a6)
{
  return a6;
}

/* a0-a5 are used by ints, the 8-byte VLS should use a6.  */
/* { dg-final { scan-assembler-times "mv\ta0,a6" 1 } } */
