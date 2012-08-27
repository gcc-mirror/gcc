/* For MIPS64r2 use DEXT rather than DSLL/DSRL to zero-extend.  */
/* { dg-do compile } */
/* { dg-options "isa_rev>=2 -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tdext\t" } } */
/* { dg-final { scan-assembler-not "\td?sll" } } */

NOMIPS16 unsigned long long
f (unsigned *i)
{
  unsigned j = *i;
  j >>= 1;			/* enforce this is all done in SI mode */
  j++;				/* don't merge the shift and the extension */
  return j;
}
