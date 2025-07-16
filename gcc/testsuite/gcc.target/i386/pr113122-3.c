/* PR target/113122 */
/* { dg-do assemble { target *-*-linux* } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-fprofile -O2 -masm=intel" } */
/* { dg-additional-options "-mfentry -fno-pic" { target *-*-gnu* } } */

void
func (void)
{
}
