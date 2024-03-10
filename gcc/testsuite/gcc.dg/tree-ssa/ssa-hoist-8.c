/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

int mem;
void foo ();
int bar (int flag)
{
  int res;
  foo ();
  /* Hoist the load of mem here even though foo () clobbers it.  */
  if (flag)
    res = mem;
  else
    {
      res = mem;
      mem = 2;
    }
  return res;
}

/* { dg-final { scan-tree-dump "HOIST inserted: 1" "pre" } } */
/* { dg-final { scan-tree-dump-times " = mem;" 1 "pre" } } */
