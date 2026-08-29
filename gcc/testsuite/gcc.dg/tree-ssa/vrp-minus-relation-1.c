/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

void dead (void);

void f (int a, int b)
{
  int d = a - b;
  if (d < 0)
    return;
  if (d > 0)
    return;
  if (a != b)
    dead ();
}

/* { dg-final { scan-tree-dump-not "dead" "vrp1" } } */
