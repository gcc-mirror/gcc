/* For PR tree-optimization/14784  */

/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fdump-tree-unswitch-details" } */

typedef struct bitmap_element_def
{
  unsigned int indx;
} bitmap_element;

typedef struct bitmap_head_def {
    bitmap_element *first;
    int using_obstack;
} bitmap_head;
typedef struct bitmap_head_def *bitmap;

bitmap_element *bitmap_free;

void foo (bitmap head, bitmap_element *elt)
{
  while (1)
    {
      /* Alias analysis problems used to prevent us from recognizing
	 that this condition is invariant.  */
      if (head->using_obstack)
	bitmap_free = elt;
    }
}


/* { dg-final { scan-tree-dump-times "Unswitching" 1 "unswitch"} } */
/* { dg-final { cleanup-tree-dump "unswitch" } } */
