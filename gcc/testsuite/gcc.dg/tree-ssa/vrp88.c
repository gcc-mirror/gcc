/* { dg-do compile } */

/* { dg-options "-O2 -fdump-tree-vrp1-details" } */


typedef const struct bitmap_head_def *const_bitmap;
typedef unsigned long BITMAP_WORD;
typedef struct bitmap_element_def {
  struct bitmap_element_def *next;
  BITMAP_WORD bits[((128 + (8 * 8 * 1u) - 1) / (8 * 8 * 1u))];
} bitmap_element;
typedef struct bitmap_head_def {
  bitmap_element *first;
} bitmap_head;
unsigned char
bitmap_single_bit_set_p (const_bitmap a)
{
  unsigned long count = 0;
  const bitmap_element *elt;
  unsigned ix;
  if ((!(a)->first))
    return 0;
  elt = a->first;
  if (elt->next != ((void *)0))
    return 0;
  for (ix = 0; ix != ((128 + (8 * 8 * 1u) - 1) / (8 * 8 * 1u)); ix++)
    {
      count += __builtin_popcountl (elt->bits[ix]);
      if (count > 1)
 return 0;
    }
  return count == 1;
}

/* Verify that VRP simplified an "if" statement.  */
/* { dg-final { scan-tree-dump "Folded into: if.*" "vrp1"} } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */


