/* PR target/113122 */
/* { dg-do assemble { target { *-*-linux* && lp64 } } } */
/* { dg-require-effective-target mfentry } */
/* { dg-require-effective-target masm_intel } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fpic -fprofile -mfentry -O2 -mcmodel=large -masm=intel" } */

void
func (void)
{
}
