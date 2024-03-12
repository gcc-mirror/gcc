/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-sink1-details" } */

void bar ();
int foo (int *p, int x)
{
  int res = *p;
  if (x)
    {
      bar ();
      res = 1;
    }
  return res;
}

/* { dg-final { scan-tree-dump "Sinking # VUSE" "sink1" } } */
