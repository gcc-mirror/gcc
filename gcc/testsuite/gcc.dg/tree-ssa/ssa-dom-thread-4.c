/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-dom1-details" } */
/* { dg-additional-options "-mbranch-cost=2" { target s390*-*-* } } */
struct bitmap_head_def;
typedef struct bitmap_head_def *bitmap;
typedef const struct bitmap_head_def *const_bitmap;
typedef unsigned long BITMAP_WORD;
typedef struct bitmap_element_def
{
  struct bitmap_element_def *next;
  unsigned int indx;
} bitmap_element;









unsigned char
bitmap_ior_and_compl (bitmap dst, const_bitmap a, const_bitmap b,
		      const_bitmap kill)
{
  unsigned char changed = 0;

  bitmap_element *dst_elt;
  const bitmap_element *a_elt, *b_elt, *kill_elt, *dst_prev;

  while (a_elt || b_elt)
    {
      unsigned char new_element = 0;

      if (b_elt)
	while (kill_elt && kill_elt->indx < b_elt->indx)
	  kill_elt = kill_elt->next;

      if (b_elt && kill_elt && kill_elt->indx == b_elt->indx
	  && (!a_elt || a_elt->indx >= b_elt->indx))
	{
	  bitmap_element tmp_elt;
	  unsigned ix;

	  BITMAP_WORD ior = 0;

	      changed = bitmap_elt_ior (dst, dst_elt, dst_prev,
					a_elt, &tmp_elt, changed);

	}

    }


  return changed;
}
/* The block starting the second conditional has  3 incoming edges,
   we should thread all three, but due to a bug in the threading
   code we missed the edge when the first conditional is false
   (b_elt is zero, which means the second conditional is always
   zero.  */
/* ARM Cortex-M defined LOGICAL_OP_NON_SHORT_CIRCUIT to false,
   so skip below test.  */
/* { dg-final { scan-tree-dump-times "Threaded" 3 "dom1" { target { ! { { mips*-*-* avr-*-* arc*-*-* } || { arm_cortex_m } } } } } } */
/* MIPS defines LOGICAL_OP_NON_SHORT_CIRCUIT to 0, so we split both
   "a_elt || b_elt" and "b_elt && kill_elt" into two conditions each,
   rather than using "(var1 != 0) op (var2 != 0)".  Also, as on other targets,
   we duplicate the header of the inner "while" loop.  There are then
   4 threading opportunities:

   1x "!a_elt && b_elt" in the outer "while" loop
      -> the start of the inner "while" loop,
	 skipping the known-true "b_elt" in the first condition.
   1x "!b_elt" in the first condition
      -> the outer "while" loop's continuation point,
	 skipping the known-false "b_elt" in the second condition.
   2x "kill_elt->indx >= b_elt->indx" in the first "while" loop
      -> "kill_elt->indx == b_elt->indx" in the second condition,
	 skipping the known-true "b_elt && kill_elt" in the second
	 condition.  */
/* Likewise for arc.  */
/* For avr, BRANCH_COST is by default 0, so the default
   LOGICAL_OP_NON_SHORT_CIRCUIT definition also computes as 0.  */
/* { dg-final { scan-tree-dump-times "Threaded" 4 "dom1" { target mips*-*-* avr-*-* arc*-*-* } } } */
/* { dg-final { cleanup-tree-dump "dom1" } } */

