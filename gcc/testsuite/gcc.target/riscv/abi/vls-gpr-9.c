/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

/* Test that VLS types > 2*XLEN (> 128 bits for rv64) are passed
   by reference, not in GPRs.  */

typedef long __attribute__((vector_size(32))) v4di;

v4di test_vls_by_reference (int a0, v4di a1)
{
  return a1;
}

/* The 32-byte VLS vector should be passed by reference.
   Return value pointer in a0, argument pointer in a2 (a1 holds a0).  */
/* { dg-final { scan-assembler "vle64.v\tv\[0-9\]+,0\\(a2\\)" } } */
/* { dg-final { scan-assembler "vse64.v\tv\[0-9\]+,0\\(a0\\)" } } */
