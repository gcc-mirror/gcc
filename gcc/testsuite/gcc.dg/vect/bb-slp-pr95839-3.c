/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-w -Wno-psabi" } */

typedef float __attribute__((vector_size(32))) v8f32;

v8f32 f(v8f32 a, v8f32 b)
{
  /* Check that we vectorize this CTOR without any loads.  */
  return (v8f32){a[0] + b[0], a[1] + b[1], a[2] + b[2], a[3] + b[3],
		 a[4] + b[4], a[5] + b[5], a[6] + b[6], a[7] + b[7]};
}

/* { dg-final { scan-tree-dump-not "from scalars" "slp2" } } */
/* { dg-final { scan-tree-dump "optimized: basic block" "slp2" } } */
