// -fno-delete-null-pointer-checks prevents nonnull_arg_p from using the
// implicit nonnull property of a C++ reference, so the conditional load must
// stay in the loop.

// { dg-do compile }
// { dg-options "-O2 -fno-delete-null-pointer-checks -fdump-tree-lim2-details" }

struct S { int n; int *p; };

int
f (S &s, int n)
{
  int r = 0;
  for (int i = 0; i < n; ++i)
    if (i & 1)
      r += s.p[i];
  return r;
}

// { dg-final { scan-tree-dump "Memory reference \[^\n\r]*->p" "lim2" } }
// { dg-final { scan-tree-dump-not "Moving statement \[^\n\r]*->p;" "lim2" } }
