/* PR target/114094 */
/* { dg-do assemble { target *-*-linux* } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-require-effective-target pie } */
/* { dg-options "-fpie -fprofile -mno-direct-extern-access -masm=intel" } */

void
foo (void)
{
}
