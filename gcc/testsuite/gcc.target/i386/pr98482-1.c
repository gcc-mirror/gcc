/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-require-effective-target mfentry } */
/* { dg-options "-fprofile -mfentry -O2 -mcmodel=large" } */
/* { dg-final { scan-assembler "movabsq\t\\\$__fentry__, %r10\n\tcall\t\\*%r10" } } */

void
func (void)
{
}
