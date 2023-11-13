/* { dg-do compile } */
/* { dg-options "-O2 -fsplit-paths -fdump-tree-split-paths-details -w" } */

typedef struct bitmap_head_def *bitmap;
extern void vec_assert_fail (const char *, const char *, const char *file_,
			     unsigned line_, const char *function_)
  __attribute__ ((__noreturn__));
typedef struct VEC_int_base
{
  unsigned num;
  unsigned alloc;
  int vec[1];
}
VEC_int_base;
static __inline__ int
VEC_int_base_space (VEC_int_base * vec_, int alloc_, const char *file_,
		    unsigned line_, const char *function_)
{
  return vec_ ? vec_->alloc - vec_->num >= (unsigned) alloc_ : !alloc_;
}

static __inline__ int *
VEC_int_base_quick_push (VEC_int_base * vec_, int obj_, const char *file_,
			 unsigned line_, const char *function_)
{
  (void) ((vec_->num <
	   vec_->alloc) ? 0 : (vec_assert_fail ("push", "VEC(int,base)",
						file_, line_, function_), 0));
}

typedef struct VEC_int_heap
{
  VEC_int_base base;
}
VEC_int_heap;
void *vec_heap_o_reserve (VEC_int_heap *, int, __SIZE_TYPE__, __SIZE_TYPE__);
int VEC_int_base_length (VEC_int_base *);
static __inline__ int
VEC_int_heap_reserve (VEC_int_heap ** vec_, int alloc_, const char *file_,
		      unsigned line_, const char *function_)
{
  int extend =
    !VEC_int_base_space (((*vec_) ? &(*vec_)->base : 0), alloc_, file_, line_,
			 function_);
  if (extend)
    *vec_ =
      (VEC_int_heap *) vec_heap_o_reserve (*vec_, alloc_,
					   __builtin_offsetof (VEC_int_heap,
							       base.vec),
					   sizeof (int));
}

static __inline__ int *
VEC_int_heap_safe_push (VEC_int_heap ** vec_, const int obj_,
			const char *file_, unsigned line_,
			const char *function_)
{
  VEC_int_heap_reserve (vec_, 1, file_, line_, function_);
  return VEC_int_base_quick_push (((*vec_) ? &(*vec_)->base : 0), obj_, file_,
				  line_, function_);
};

typedef struct bitmap_head_def
{
}
bitmap_head;
typedef struct
{
}
bitmap_iterator;
int bmp_iter_and_compl_init (bitmap_iterator *, bitmap_head *, bitmap, int,
			     int *);
int bmp_iter_and_compl (bitmap_iterator *, int *);
void bmp_iter_next (bitmap_iterator *, int *);
void VEC_int_heap_free (VEC_int_heap **);
bitmap
compute_idf (bitmap_head * dfs)
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
	  (VEC_int_heap_safe_push
	   (&(work_stack), i, "/home/gcc/virgin-gcc/gcc/cfganal.c", 1349,
	    __FUNCTION__));
	}
    }
  (VEC_int_heap_free (&work_stack));
}

/* { dg-final { scan-tree-dump-not "Duplicating join block" "split-paths" } } */
