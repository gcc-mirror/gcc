/* { dg-additional-options "-fpermissive" } */

typedef int *loop_p;
typedef struct VEC_loop_p_base
{
  unsigned num;
  loop_p vec[1];
}
VEC_loop_p_base;

__inline__ int
VEC_loop_p_base_iterate (const VEC_loop_p_base * vec_, unsigned ix_,
			 loop_p * ptr)
{
  if (vec_ && ix_ < vec_->num)
    {
      *ptr = vec_->vec[ix_];
      return 1;
    }
  else
    {
      return 0;
    }
}

typedef struct VEC_loop_p_heap
{
  VEC_loop_p_base base;
}
VEC_loop_p_heap;


static __inline__ int
am_vector_index_for_loop (VEC_loop_p_heap * loop_nest, int loop_num)
{
  int i;
  loop_p l;

  for (i = 0;
       VEC_loop_p_base_iterate ((loop_nest) ? &(loop_nest)->base : 0, i,
				&(l)); i++)
    if (l == loop_num)
      return i;

  __builtin_unreachable ();
}

unsigned char
build_access_matrix (unsigned max)
{
  unsigned i;
  for (i = 0; i < max; i++)
    {
      if (am_vector_index_for_loop (foo (), 0))
	return 0;
    }
}
