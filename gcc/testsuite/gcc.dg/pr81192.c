/* { dg-options "-Os -fdump-tree-pre-details" } */

unsigned a;
int b, c;

static int
fn1 (int p1, int p2)
{
  return p1 > 2147483647 - p2 ? p1 : p1 + p2;
}

void
fn2 (void)
{
  int j;
  a = 30;
  for (; a;)
    for (; c; b = fn1 (j, 1))
      ;
}

/* { dg-final { scan-tree-dump-times "(?n)find_duplicates: <bb .*> duplicate of <bb .*>" 1 "pre" } } */
