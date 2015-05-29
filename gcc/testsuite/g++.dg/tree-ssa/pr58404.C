// { dg-do compile }
// { dg-options "-O -fdump-tree-cddce1" }

struct S { int s; };
S a[1024];

void
foo ()
{
  for (int i = 0; i < 1024; i++)
    {
      S &r = a[i];
      r.s++;
    }
}

// We should propagate the reference into both memory accesses
// during the first forwprop pass
// { dg-final { scan-tree-dump-times "= &a" 0 "cddce1" } }
