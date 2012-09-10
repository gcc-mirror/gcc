/* For MIPS64r2 use DEXT rather than DSLL/DSRL for clear_upper32.  */
/* { dg-do compile } */
/* { dg-options "isa_rev>=2 -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tdext\t" } } */
/* { dg-final { scan-assembler-not "\td?sll" } } */

NOMIPS16 unsigned long long
f (unsigned long long i)
{
  return i & 0xffffffffull;
}
