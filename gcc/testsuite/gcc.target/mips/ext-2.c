/* Turn the truncate,zero_extend,lshiftrt sequence before the or into a
   zero_extract.  The truncate is due to TARGET_PROMOTE_PROTOTYPES, the
   zero_extend to PROMOTE_MODE.  */
/* { dg-do compile } */
/* { dg-options "-O isa_rev>=2 -mgp64" } */
/* { dg-final { scan-assembler "\tdext\t" } } */
/* { dg-final { scan-assembler-not "and" } } */
/* { dg-final { scan-assembler-not "srl" } } */

void
f (unsigned char x, unsigned char *r)
{
  *r = 0x50 | (x >> 4);
}
