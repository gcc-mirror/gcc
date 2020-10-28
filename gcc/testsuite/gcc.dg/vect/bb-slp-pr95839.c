/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-w -Wno-psabi" } */

typedef float __attribute__((vector_size(16))) v4f32;

v4f32 f(v4f32 a, v4f32 b)
{
  /* Check that we vectorize this CTOR without any loads.  */
  return (v4f32){a[0] + b[0], a[1] + b[1], a[2] + b[2], a[3] + b[3]};
}

/* { dg-final { scan-tree-dump "optimized: basic block" "slp2" } } */
