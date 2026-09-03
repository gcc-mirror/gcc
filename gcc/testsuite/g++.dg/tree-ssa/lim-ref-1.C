// A parameter of reference type is bound to a valid object of the referenced
// type, so a load from it cannot trap and may be hoisted out of the loop even
// though it is only executed on some iterations.

// { dg-do compile }
// { dg-options "-O2 -fdelete-null-pointer-checks -fdump-tree-lim2-details" }
// { dg-skip-if "" keeps_null_pointer_checks }

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

// { dg-final { scan-tree-dump "Moving statement \[^\n\r]*->p;" "lim2" } }
