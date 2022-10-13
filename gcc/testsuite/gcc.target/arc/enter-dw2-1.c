/* Verify that we generate appropriate CFI offsets in the case of enter
   instruction.  */
/* { dg-skip-if "Not having enter_s insn." { arc700 || arc6xx } } */
/* { dg-do compile } */
/* { dg-options "-g -Os" } */

extern void bar (void);

void foo (void)
{
  asm volatile (";my clobber list"
		: : : "r13", "r14", "r15", "r16", "r17", "r18", "r19");
  bar ();
  asm volatile (";my clobber list"
		: : : "r13", "r14", "r15", "r16", "r17", "r18", "r19");
}


/* { dg-final { scan-assembler-times "enter_s" 1 } } */
/* { dg-final { scan-assembler-times "\.cfi_def_cfa_offset 32" 1 } } */
/* { dg-final { scan-assembler-times "\.cfi_offset 31, -32" 1 } } */
/* { dg-final { scan-assembler-times "\.cfi_offset 13, -28" 1 } } */
/* { dg-final { scan-assembler-times "\.cfi_offset 14, -24" 1 } } */
/* { dg-final { scan-assembler-times "\.cfi_offset 15, -20" 1 } } */
/* { dg-final { scan-assembler-times "\.cfi_offset 16, -16" 1 } } */
/* { dg-final { scan-assembler-times "\.cfi_offset 17, -12" 1 } } */
/* { dg-final { scan-assembler-times "\.cfi_offset 18, -8" 1 } } */
/* { dg-final { scan-assembler-times "\.cfi_offset 19, -4" 1 } } */
