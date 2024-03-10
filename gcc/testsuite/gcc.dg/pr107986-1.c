/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void bar ();
void foo (int *a)
{
  int qa = 0;
  for (int i = 0; i < 3; i++)
    if (a[i])
      a[qa++] = 0;
/* Show that we know qa is not negative. */
  if (qa < 0)
    bar ();
}

/* { dg-final { scan-tree-dump-not "bar" "evrp" } } */
