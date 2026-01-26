/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O2" } */

/* Test stack split with two 8-byte VLS arguments on rv32.
   First VLS splits between a7 and stack, second is fully on stack.  */

typedef int __attribute__((vector_size(8))) v2si;

v2si test_vls_gpr_stack_split2_rv32 (int a0, int a1, int a2, int a3,
				     int a4, int a5, int a6, v2si a7,
				     v2si a8)
{
  v2si res = a7 + a8;
  return res;
}

/* a7 splits (4 bytes in a7, 4 on stack), a8 fully on stack.  */
/* { dg-final { scan-assembler "sw\ta7," } } */
/* { dg-final { scan-assembler "lw\t\[at\]\[0-9\]+,\[0-9\]+\\(sp\\)" } } */
