/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-thread3-details -fdump-tree-thread4-details -fno-finite-loops --param early-inlining-insns=14 -fno-inline-functions" } */
/* { dg-final { scan-tree-dump "Registering jump thread" "thread3" } } */
/* { dg-final { scan-tree-dump "Registering jump thread" "thread4" } } */

typedef struct bitmap_head_def *bitmap;
typedef const struct bitmap_head_def *const_bitmap;
typedef struct VEC_int_base
{
}
VEC_int_base;
typedef struct VEC_int_heap
{
  VEC_int_base base;
}
VEC_int_heap;
typedef unsigned long BITMAP_WORD;
typedef struct bitmap_element_def
{
  struct bitmap_element_def *next;
  unsigned int indx;
}
bitmap_element;
typedef struct bitmap_head_def
{
}
bitmap_head;
typedef struct
{
  bitmap_element *elt1;
  bitmap_element *elt2;
  BITMAP_WORD bits;
}
bitmap_iterator;
static __inline__ void
bmp_iter_and_compl_init (bitmap_iterator * bi, const_bitmap map1,
			 const_bitmap map2, unsigned start_bit,
			 unsigned *bit_no)
{
}

static __inline__ void
bmp_iter_next (bitmap_iterator * bi, unsigned *bit_no)
{
}

static __inline__ unsigned char
bmp_iter_and_compl (bitmap_iterator * bi, unsigned *bit_no)
{
  if (bi->bits)
    {
      while (bi->elt2 && bi->elt2->indx < bi->elt1->indx)
	bi->elt2 = bi->elt2->next;
    }
}

extern int VEC_int_base_length (VEC_int_base *);
bitmap
compute_idf (bitmap def_blocks, bitmap_head * dfs)
{
  bitmap_iterator bi;
  unsigned bb_index, i;
  VEC_int_heap *work_stack;
  bitmap phi_insertion_points;
  while ((VEC_int_base_length (((work_stack) ? &(work_stack)->base : 0))) > 0)
    {
      for (bmp_iter_and_compl_init
	   (&(bi), (&dfs[bb_index]), (phi_insertion_points), (0), &(i));
	   bmp_iter_and_compl (&(bi), &(i)); bmp_iter_next (&(bi), &(i)))
	{
	}
    }
}
