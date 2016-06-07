/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate-blocks-details" } */

void g(void);
void h(void);
void f(int x, int y)
{
  if (x) goto A;
  if (y) goto B;
  return;

 A: __attribute__((cold))
  g();
  return;

 B: __attribute__((hot))
  h();
  return;
}

/* { dg-final { scan-tree-dump-times 1 "hot label heuristics" 1 "profile_estimate" } } */
/* { dg-final { scan-tree-dump-times 1 "cold label heuristics" 1 "profile_estimate" } } */
/* { dg-final { scan-tree-dump-times "block 4, loop depth 0, count 0, freq \[1-4\]\[^0-9\]" 1 "profile_estimate" } } */

/* Note: we're attempting to match some number > 6000, i.e. > 60%.
   The exact number ought to be tweekable without having to juggle
   the testcase around too much.  */
/* { dg-final { scan-tree-dump-times "block 5, loop depth 0, count 0, freq \[6-9\]\[0-9\]\[0-9\]\[0-9\]" 1 "profile_estimate" } } */

