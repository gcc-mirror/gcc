/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

/* Test that 16-byte VLS is split between GPR and stack when only
   one GPR is available.  Per the psABI: "if only one register is
   available, the first XLEN bits are passed in a register and the
   remaining bits are passed on the stack."  */

typedef long __attribute__((vector_size(16))) v2di;

v2di test_vls_gpr_stack_split2 (int a0, int a1, int a2, int a3,
				int a4, int a5, int a6, v2di a7,
				v2di a8)
{
  v2di res = a7 + a8;
  return res;
}

/* a0-a6 use 7 GPRs, leaving only a7.  The 16-byte VLS should be
   split: first 8 bytes in a7, remaining 8 bytes on stack.  */
/* { dg-final { scan-assembler "sd\ta7," } } */
