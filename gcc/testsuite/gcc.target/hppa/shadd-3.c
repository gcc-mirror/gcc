/* { dg-do compile }  */
/* { dg-options "-O2 -fno-delayed-branch" }  */
/* In this test we want to verify that combine canonicalizes the
   MULT into an ASHIFT which in turn allows postreload-gcse to
   find the common subexpression.

   Neither pass dumps stuff in a format that is particularly good
   for parsing here, so we count the shadd insns.  More is not
   necessarily better in this test.  If this test is too fragile
   over time we'll have to revisit the combine and/or postreload
   dumps.  Note we have disabled delay slot filling to improve
   test stability.  */
/* { dg-final { scan-assembler-times "sh.add" 4 } }  */

extern void oof (void);
typedef struct simple_bitmap_def *sbitmap;
struct simple_bitmap_def
{
  unsigned char *popcount;
  unsigned int n_bits;
  unsigned long elms[1];
};
__inline__ void
SET_BIT (sbitmap map, unsigned int bitno)
{
  if (map->popcount)
    {
      unsigned char oldbit;
      oldbit =
	((map)->elms[bitno / 64]);
      if (!oldbit)
	oof ();
    }
  map->elms[bitno / 64] |= 1;
}

void
fix_bb_placements (int indx1, int indx2, sbitmap in_queue)
{
  SET_BIT (in_queue, indx1);
  SET_BIT (in_queue, indx2);
}
