/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

/* Test that 4-byte VLS types (< XLEN) are passed in a single GPR.  */

typedef short __attribute__((vector_size(4))) v2hi;

v2hi test_vls_small (int a0, v2hi a1)
{
  return a1;
}

/* The 4-byte VLS vector should be passed in a1.  */
/* { dg-final { scan-assembler-times "mv\ta0,a1" 1 } } */
