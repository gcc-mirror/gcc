/* { dg-do compile } */
// { dg-options "-O2 -fdump-tree-optimized" }


struct f
{
  char m;
  char m1;
};

static inline char f:: *g(int a)
{
  return a?0:&f::m;
}

int h(void)
{
  char f:: *a = g(0);
  return a == 0;
}

/* We should have no cast to offset_type. */
/* { dg-final { scan-tree-dump-times "offset_type" 0 "optimized"} } */
// And we should optimized this code to just return 0
/* { dg-final { scan-tree-dump-times "return 0" 1 "optimized"} } */

