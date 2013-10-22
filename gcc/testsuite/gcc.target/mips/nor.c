/* { dg-do compile } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-times "\tnor\t" 1 } } */
/* { dg-final { scan-assembler-not "\tor" } } */

/* Test that we generate a 'nor' instruction and no 'or' instructions.  */

NOMIPS16 int f (int a, int b)
{
	return ~(a|b);
}
