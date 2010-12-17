/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mtune=generic -dp" } */

typedef struct bitmap_element_def {
  struct bitmap_element_def *next;
  unsigned int indx;
} bitmap_element;
typedef struct bitmap_head_def {
  bitmap_element *first;
  bitmap_element *current;
  unsigned int indx;
} bitmap_head;
typedef struct bitmap_head_def *bitmap;
typedef const struct bitmap_head_def *const_bitmap;
extern void bar (void) __attribute__ ((__noreturn__));
unsigned char
bitmap_and_compl_into (bitmap a, const_bitmap b)
{
  bitmap_element *a_elt = a->first;
  const bitmap_element *b_elt = b->first;
  if (a == b)
    {
      if ((!(a)->first))
	return 0;
      else
	return 1;
    }
  while (a_elt && b_elt)
    {
      if (a_elt->indx < b_elt->indx)
	a_elt = a_elt->next;
    }
  if (a->indx == a->current->indx)
    bar ();
  return 0;
}

/* { dg-final { scan-assembler-not "avx_vzeroupper" } } */
