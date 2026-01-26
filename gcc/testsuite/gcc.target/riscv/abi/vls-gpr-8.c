/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O2" } */

/* Test that 8-byte VLS types (= 2*XLEN for rv32) are passed in 2 GPRs.  */

typedef int __attribute__((vector_size(8))) v2si;

v2si test_vls_two_gprs_rv32 (int a0, v2si a1)
{
  return a1;
}

/* The 8-byte VLS vector is passed in a1,a2 and returned in a0,a1 on rv32.  */
/* { dg-final { scan-assembler "mv\ta0,a1" } } */
/* { dg-final { scan-assembler "mv\ta1,a2" } } */
