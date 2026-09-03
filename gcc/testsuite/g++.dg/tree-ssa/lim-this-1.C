// -fno-delete-null-pointer-checks prevents nonnull_arg_p from using the
// implicit nonnull property of a method's this parameter, so the conditional
// load must stay in the loop.

// { dg-do compile }
// { dg-options "-O2 -fno-delete-null-pointer-checks -fdump-tree-lim2-details" }

struct S
{
  int *values;
  int f (int);
};

int
S::f (int n)
{
  int r = 0;
  for (int i = 0; i < n; ++i)
    if (i & 1)
      r += values[i];
  return r;
}

// { dg-final { scan-tree-dump "Memory reference \[^\n\r]*->values" "lim2" } }
// { dg-final { scan-tree-dump-not "Moving statement \[^\n\r]*->values;" "lim2" } }
