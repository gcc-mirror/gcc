/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-require-effective-target mfentry } */
/* { dg-options "-fprofile -mfentry -O2 -mcmodel=large" } */
/* { dg-final { scan-assembler "movabsq\t\\\$__fentry__, %r10\n\tcall\t\\*%r10" { target nonpic } } } */
/* { dg-final { scan-assembler "movabsq\t\\\$__fentry__@PLTOFF, %r11\n\taddq\t%r11, %r10\n\tcall\t\\*%r10" { target { ! nonpic } } } } */

void
func (void)
{
}
