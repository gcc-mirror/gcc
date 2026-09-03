// The referenced type bounds what may be speculated: an access past the end
// of the referenced object still counts as trapping and stays in the loop.

// { dg-do compile }
// { dg-options "-O2 -fdelete-null-pointer-checks -fdump-tree-lim2-details" }
// { dg-skip-if "" keeps_null_pointer_checks }

struct Small { int a; };
struct Big { int a; long b[8]; };

long
g (Small &s, int n)
{
  long r = 0;
  for (int i = 0; i < n; ++i)
    if (i & 1)
      r += reinterpret_cast<Big &> (s).b[3];
  return r;
}

// { dg-final { scan-tree-dump "Memory reference \[^\n\r]*struct Big" "lim2" } }
// { dg-final { scan-tree-dump-not "Moving statement \[^\n\r]*struct Big" "lim2" } }
