/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O2" } */

/* Test that 8-byte VLS is split between GPR and stack when only
   one GPR is available on rv32.  Per the psABI: "if only one register
   is available, the first XLEN bits are passed in a register and the
   remaining bits are passed on the stack."  */

typedef int __attribute__((vector_size(8))) v2si;

v2si test_vls_gpr_stack_split_rv32 (int a0, int a1, int a2, int a3,
				    int a4, int a5, int a6, v2si a7)
{
  return a7;
}

/* a0-a6 use 7 GPRs, leaving only a7.  The 8-byte VLS should be
   split: first 4 bytes in a7, remaining 4 bytes on stack.  */
/* { dg-final { scan-assembler "sw\ta7," } } */
