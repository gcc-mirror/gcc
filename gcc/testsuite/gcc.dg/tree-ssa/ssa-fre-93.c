/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

void bar ();
void foo (int pred, int *other)
{
  *other = 0;
  if (*other)
    goto cnt;
  if (pred)
    {
      *other = 1;
cnt:
      if (!pred)
        bar ();
    }
}

/* The first VN pass should figure that if (!pred) is false because
   if (*other) is and thus the predicate test is redundant.  */
/* { dg-final { scan-tree-dump-not "bar" "fre1" } } */
