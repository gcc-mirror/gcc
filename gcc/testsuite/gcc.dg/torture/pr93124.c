/* { dg-additional-options "-fno-rerun-cse-after-loop -fno-guess-branch-probability -fno-tree-fre" } */

int x;

void fn2 ();
void fn3 ();
void fn4 ();
void fn5 ();
void fn6 ();

void
fn1 (void)
{
  int n;
  for (n = 0;; ++n) {
    {
      struct { char a[n]; } s;
      fn2 (s);
    }
    struct { unsigned a[x]; } s;
    int i, b;
    for (i = 0; i < n; ++i)
      ;
    fn2 (s);
    {
      struct { char a[n]; } s;
      int i;
      for (i = 0; i < n; ++i)
        s.a[i] = i;
      fn3 (s, s);
    }
    fn4 ();
    {
      struct { unsigned a[n]; } s;
      fn5 (s);
    }
    {
      struct { char a[b]; } s;
      for (; i < n;)
        s.a[i] = i;
      fn6 (s);
    }
  }
}
