/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fomit-frame-pointer -march=atom -S" } */
/* { dg-final { scan-assembler-times "nop; nop; nop; nop" 1 } } */
/* { dg-final { scan-assembler-not "nop; nop; nop; nop; nop" } } */
/* { dg-final { scan-assembler-not "rep" } } */

extern void bar (void);

void
foo (int x)
{
  if (x)
    bar ();
}
