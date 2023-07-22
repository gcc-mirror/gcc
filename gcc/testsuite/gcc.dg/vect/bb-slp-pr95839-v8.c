/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect64 } */
/* { dg-additional-options "-w -Wno-psabi" } */

typedef float __attribute__((vector_size(8))) v2f32;

v2f32 f(v2f32 a, v2f32 b)
{
  /* Check that we vectorize this CTOR without any loads.  */
  return (v2f32){a[0] + b[0], a[1] + b[1]};
}

/* { dg-final { scan-tree-dump "optimized: basic block" "slp2" } } */
