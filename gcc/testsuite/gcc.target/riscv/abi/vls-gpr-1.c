/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

/* Test that small VLS types (<= 2*XLEN = 128 bits) are passed in GPRs
   per the psABI, not by reference.  */

typedef int __attribute__((vector_size(8))) v2si;

v2si test_vls_in_gpr (int a0, int a1, v2si a2)
{
  return a2;
}

/* The 8-byte VLS vector should be passed in a2 and returned in a0.  */
/* { dg-final { scan-assembler-times "mv\ta0,a2" 1 } } */
