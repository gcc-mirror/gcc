/* PR target/113122 */
/* { dg-do assemble { target *-*-linux* } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fpic -fprofile -O2 -masm=intel" } */

void
func (void)
{
}
