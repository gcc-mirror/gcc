/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre-details" } */

union U {
  int i;
  float f;
};
int foo(int i, int b)
{
  union U u;
  if (b)
    {
      i = i << 2;
      u.i = i;
      return u.f;
    }
  else
    {
      i = i << 2;
      u.i = i;
      return u.f;
    }
}

/* { dg-final { scan-tree-dump-times "Replaced u.f with pretmp" 2 "fre" } } */
/* { dg-final { scan-tree-dump-times "Inserted pretmp" 2 "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
