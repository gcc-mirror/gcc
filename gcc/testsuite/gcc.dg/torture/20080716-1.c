/* { dg-do run } */
/* { dg-require-effective-target lp64 } */

typedef unsigned long size_t;
struct tree_base
{
  int code;
};
struct tree_decl_minimal
{
  struct tree_base base;
  const char *name;
};
typedef union tree_node {
  struct tree_base base;
  struct tree_decl_minimal decl_minimal;
} *tree;
struct tree_overload
{
  struct tree_base common;
  tree function;
};
typedef struct VEC_tree_base { unsigned num; unsigned alloc; tree vec[1]; } VEC_tree_base;
typedef struct VEC_tree_gc { VEC_tree_base base; } VEC_tree_gc;
static __inline__ unsigned VEC_tree_base_length (const VEC_tree_base *vec_)
{ return vec_ ? vec_->num : 0; }
static __inline__ int VEC_tree_base_iterate (const VEC_tree_base *vec_, unsigned ix_, tree *ptr)
{
  if (vec_ && ix_ < vec_->num) { *ptr = vec_->vec[ix_]; return 1; } else { *ptr = 0; return 0; }
} 
extern void abort (void);
void __attribute__((noinline)) foo (size_t x)
{
  if (x != 18446744073709551614UL)
    abort ();
}
void
resort_type_method_vec (VEC_tree_gc *method_vec)
{
  int len = (VEC_tree_base_length(((method_vec) ? &(method_vec)->base : 0)));
  size_t slot;
  tree fn;

  for (slot = 2;
       (VEC_tree_base_iterate(((method_vec) ? &(method_vec)->base : 0),slot,&(fn)));
       ++slot)
    if (!(((((((fn)->base.code) == 225) ? (((struct tree_overload*)(fn))->function) : (fn)))->decl_minimal.name)))
      break;

  if (len - slot > 1)
    foo (len - slot);
}

int main ()
{
  resort_type_method_vec ((void *)0);
  return 0;
}
