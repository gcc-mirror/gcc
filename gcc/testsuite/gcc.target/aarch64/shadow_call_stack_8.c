/* Verify:
     * -fomit-frame-pointer -fsanitize=shadow-call-stack -fno-exceptions -ffixed-x18.
     * without outgoing.
     * total frame <= 512 but > 256.
     * callee-saved reg: x19, x20, x30.
     * optimized code should use "stp   x19, x20, [sp, -x]!" to save x19, x20 in prologue.
     * optimized code should use "str	x30, [sp " to save x30 in prologue.
     * optimized code should use "ldp	x19, x20, [sp], x" to retore x19, x20 in epilogue.
     * optimized code should not restore x30 in epilogue.  */

/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer -fsanitize=shadow-call-stack -fno-exceptions -ffixed-x18 --save-temps -fno-stack-protector" } */

int func1 (void)
{
  unsigned char a[200];
  __asm__ ("":::"x19","x20","x30");
  return 0;
}

/* { dg-final { scan-assembler-times {stp\tx19, x20, \[sp, -[0-9]+\]!} 1 } } */
/* { dg-final { scan-assembler-times {str\tx30, \[sp} 1 } } */
/* { dg-final { scan-assembler {ldp\tx19, x20, \[sp\], [0-9]+} } } */
/* { dg-final { scan-assembler-not {ld[r|p]\tx30, \[sp} } } */
