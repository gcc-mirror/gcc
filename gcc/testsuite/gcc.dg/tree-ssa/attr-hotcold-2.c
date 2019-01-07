/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

int v1, v2;
void f(int x, int y)
{
  if (x) goto A;
  if (y) goto B;
  return;

 A: __attribute__((cold))
  v1 = x;
  return;

 B: __attribute__((hot))
  v2 = y;
  return;
}

/* { dg-final { scan-tree-dump-times "hot label heuristics" 1 "profile_estimate" } } */
/* { dg-final { scan-tree-dump-times "cold label heuristics" 1 "profile_estimate" } } */
/* { dg-final { scan-tree-dump-times "combined heuristics: 10.00%" 1 "profile_estimate" } } */

/* Note: we're attempting to match some number > 6000, i.e. > 60%.
   The exact number ought to be tweekable without having to juggle
   the testcase around too much.  */
/* { dg-final { scan-tree-dump-times "combined heuristics: 90.00%" 1 "profile_estimate" } } */
