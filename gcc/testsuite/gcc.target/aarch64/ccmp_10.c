/* { dg-do compile } */
/* { dg-options "-O2 --param tree-reassoc-width=2 -fdump-tree-reassoc2-details" } */

/* A vector comparison does not set the condition flags, so a chain of them
   is not a conditional compare sequence and keeps the parallel form.  */

typedef int v4si __attribute__ ((vector_size (16)));

v4si
vector_chain (v4si a, v4si b, v4si c, v4si d, v4si e, v4si f, v4si g, v4si h)
{
  return (a < b) & (c < d) & (e < f) & (g < h);
}

/* A chain of scalar comparisons becomes one sequence instead.  */

int
scalar_chain (int a, int b, int c, int d, int e, int f, int g, int h)
{
  return (a < b) & (c < d) & (e < f) & (g < h);
}

/* A nested integer IOR is not a conditional compare sequence.  */

int
integer_ior (int a, int b, int c, int d, int e, int f, int g, int h)
{
  return (a | b) & (c < d) & (e < f) & (g < h);
}

/* { dg-final { scan-tree-dump-times "was chosen for reassociation" 1 "reassoc2" } } */
/* { dg-final { scan-assembler-times {\tccmp\t} 5 } } */
/* { dg-final { scan-assembler-times {\tand\tw[0-9]+, w[0-9]+, w[0-9]+} 1 } } */
