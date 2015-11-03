/* Check that no interrupt-specific register saves are generated.
   The function call will require to load the address first into a register,
   then use that for a jsr or jmp.  It will also need to load a constant
   address in order to load fpscr.  */
/* { dg-do compile { target { { any_fpu } && nonpic } } }  */
/* { dg-options "-O" }  */
/* { dg-final { scan-assembler-times "rte" 1 } }  */
/* { dg-final { scan-assembler-not "mov.l\tr\[0-9\],@-r15" } }  */
/* { dg-final { scan-assembler-not "mov.l\tr1\[0-4\],@-r15" } }  */
/* { dg-final { scan-assembler-not "macl" } }  */

/* Expect that fpscr needs to be saved, loaded and restored.  */
/* { dg-final { scan-assembler-times "\[^_\]fpscr" 4 } }  */

extern void foo (void);

#pragma trapa
void
isr (void)
{
  foo ();
}
