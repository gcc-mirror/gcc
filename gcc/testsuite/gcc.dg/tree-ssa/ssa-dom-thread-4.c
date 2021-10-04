/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-vrp-thread2-details -fdump-tree-dom2-details -std=gnu89 --param logical-op-non-short-circuit=1" } */
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
/* We used to catch 3 jump threads in vrp-thread1, but they all
   rotated the loop, so they were disallowed.  This in turn created
   other opportunities for the other threaders which result in the the
   post-loop threader (vrp-thread2) catching more.  */
/* { dg-final { scan-tree-dump-times "Registering jump thread" 5 "vrp-thread2" } } */
