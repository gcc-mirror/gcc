/* { dg-do compile } */
/* { dg-options "-O0" } */

/* Verify that a naked function does not spill incoming arguments onto the
   stack and does not build a frame or emit a return sequence, even when
   there are more arguments than argument registers.  */
void
__attribute__((naked))
foo (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j)
{
  __asm__ volatile ("# naked-many-args");
}

/* { dg-final { scan-assembler "# naked-many-args" } } */
/* { dg-final { scan-assembler-not "(?n)^\\s*addi\\." } } */
/* { dg-final { scan-assembler-not "(?n)^\\s*st\\." } } */
/* { dg-final { scan-assembler-not "(?n)^\\s*jr\\t\\\$r1" } } */
