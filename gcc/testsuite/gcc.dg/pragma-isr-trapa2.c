/* { dg-do compile { target sh-*-* sh4*-*-*} } */
/* { dg-options "-O -m4" } */

extern void foo ();
#pragma trapa
void
isr()
{
  foo ();
}

/* { dg-final { scan-assembler-times "rte" 1} } */
/* No interrupt-specific saves should be needed.
   The function call will require to load the address first into a register,
   then use that for a jsr or jmp.  It will also need to load a constant
   address in order to load fpscr.  */
/* { dg-final { scan-assembler-times "r\[0-7\]\n" 3 } } */
/* { dg-final { scan-assembler-not "r\[8-9\]" } } */
/* { dg-final { scan-assembler-not "r1\[,0-3\]" } } */
/* { dg-final { scan-assembler-not "macl" } } */
/* fpscr needs to be saved, loaded and restored.  */
/* { dg-final { scan-assembler-times "\[^_\]fpscr" 3 } } */
