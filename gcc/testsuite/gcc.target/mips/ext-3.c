/* For MIPS64r2 use DEXT rather than DSLL/DSRL to zero-extend.  */
/* { dg-do compile } */
/* { dg-options "-O isa_rev>=2 -mgp64" } */
/* { dg-final { scan-assembler "\tdext\t" } } */
/* { dg-final { scan-assembler-not "sll" } } */

NOMIPS16 unsigned long long
f (unsigned *i)
{
  unsigned j = *i;
  j >>= 1;			/* enforce this is all done in SI mode */
  j++;				/* don't merge the shift and the extension */
  return j;
}
