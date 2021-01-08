/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-require-effective-target mfentry } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fpic -fprofile -mfentry -O2 -mcmodel=large" } */
/* { dg-final { scan-assembler "movabsq\t\\\$__fentry__@PLTOFF, %r11\n\taddq\t%r11, %r10\n\tcall\t\\*%r10" } } */

void
func (void)
{
}
