/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O2" } */

/* Test that VLS types > 2*XLEN (> 64 bits for rv32) are passed
   by reference, not in GPRs.  */

typedef int __attribute__((vector_size(16))) v4si;

v4si test_vls_by_reference_rv32 (int a0, v4si a1)
{
  return a1;
}

/* The 16-byte VLS vector should be passed by reference on rv32
   (since 16 bytes > 2*4 = 8 bytes).  */
/* { dg-final { scan-assembler "vle32.v\tv\[0-9\]+,0\\(a2\\)" } } */
/* { dg-final { scan-assembler "vse32.v\tv\[0-9\]+,0\\(a0\\)" } } */
