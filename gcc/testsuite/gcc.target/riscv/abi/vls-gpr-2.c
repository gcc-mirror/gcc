/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

/* Test that 16-byte VLS types (= 2*XLEN for rv64) are passed in 2 GPRs.  */

typedef long __attribute__((vector_size(16))) v2di;

v2di test_vls_two_gprs (int a0, v2di a1)
{
  return a1;
}

/* The 16-byte VLS vector is passed in a1,a2 and returned in a0,a1.  */
/* { dg-final { scan-assembler "sd\ta1," } } */
/* { dg-final { scan-assembler "sd\ta2," } } */
