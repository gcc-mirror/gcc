/* Check that no interrupt-specific register saves are generated.
   The function call will require to load the address first into a register,
   then use that for a jsr or jmp.  It will also need to load a constant
   address in order to load fpscr.  */
/* { dg-do compile { target { { "sh*-*-*" } && nonpic } } }  */
/* { dg-skip-if "" { "sh*-*-*" } { "-m1" "-m2" "-m3" "-m4al" "*nofpu" "-m4-340*" "-m4-400*" "-m4-500*" "-m5*" } { "" } }  */
/* { dg-options "-O" }  */
/* { dg-final { scan-assembler-times "rte" 1 } }  */
/* { dg-final { scan-assembler-times "r\[0-7\]\n" 3 } }  */
/* { dg-final { scan-assembler-not "r\[8-9\]" } }  */
/* { dg-final { scan-assembler-not "r1\[,0-3\]" } }  */
/* { dg-final { scan-assembler-not "macl" } }  */

/* Expect that fpscr needs to be saved, loaded and restored.  */
/* { dg-final { scan-assembler-times "\[^_\]fpscr" 3 } }  */

extern void foo (void);

#pragma trapa
void
isr (void)
{
  foo ();
}
