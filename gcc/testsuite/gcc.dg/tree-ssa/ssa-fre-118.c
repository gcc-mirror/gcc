/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

struct S { int a, b; } s, t;

int f (int c, int d, float *fp)
{
  s.a = 1;
  if (c)
    {
      *fp = 1.0f;
      if (d)
	fp[1] = 2.0f;
    }
  /* The lookup of t.a is translated to s.a here.  The walk over the
     PHI merging the three paths then visits the path through the
     outer if twice which must not cause the walk to fail.  */
  t = s;
  return t.a;
}

/* { dg-final { scan-tree-dump "return 1;" "fre1" } } */
