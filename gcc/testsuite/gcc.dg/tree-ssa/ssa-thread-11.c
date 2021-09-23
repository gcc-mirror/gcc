/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp2-details --param logical-op-non-short-circuit=1" } */
/* { dg-additional-options "-fdisable-tree-ethread -fdisable-tree-thread1 -fdisable-tree-thread2" } */
/* { dg-final { scan-tree-dump-not "IRREDUCIBLE_LOOP" "vrp2" } } */

void abort (void);
typedef struct bitmap_head_def *bitmap;
typedef const struct bitmap_head_def *const_bitmap;
typedef struct bitmap_obstack
{
  struct bitmap_obstack *next;
  unsigned int indx;
}
bitmap_element;
typedef struct bitmap_head_def
{
  bitmap_element *first;
}
bitmap_head;
static __inline__ unsigned char
bitmap_elt_ior (bitmap dst, bitmap_element * dst_elt,
		bitmap_element * dst_prev, const bitmap_element * a_elt,
		const bitmap_element * b_elt)
{
  ((void) (!(a_elt || b_elt) ? abort (), 0 : 0));
}

unsigned char
bitmap_ior_and_compl (bitmap dst, const_bitmap a, const_bitmap b,
		      const_bitmap kill)
{
  bitmap_element *dst_elt = dst->first;
  const bitmap_element *a_elt = a->first;
  const bitmap_element *b_elt = b->first;
  const bitmap_element *kill_elt = kill->first;
  bitmap_element *dst_prev = ((void *) 0);
  while (a_elt || b_elt)
    {
      if (b_elt && kill_elt && kill_elt->indx == b_elt->indx
	  && (!a_elt || a_elt->indx >= b_elt->indx));
      else
	{
	  bitmap_elt_ior (dst, dst_elt, dst_prev, a_elt, b_elt);
	  if (a_elt && b_elt && a_elt->indx == b_elt->indx)
	    ;
	  else if (a_elt && (!b_elt || a_elt->indx <= b_elt->indx))
	    a_elt = a_elt->next;
	}
    }
}
