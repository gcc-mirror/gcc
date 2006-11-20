/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized -w" } */
/* Check that SRA does non block copies for structs that just contain vectors. */

#define vector __attribute__((vector_size(16)))

struct vt
{
  vector int t;
};


vector int f(vector int t1, vector int t2)
{
  struct vt st1, st2, st3;
  st1.t = t1;
  st2 = st1;
  st2.t += t2;
  st3 = st2;
  return st3.t;
}

/* There should be no references to st as SRA should not have done block copy. */
/* { dg-final { scan-tree-dump-times "st" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

