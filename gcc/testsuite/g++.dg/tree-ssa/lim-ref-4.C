// The object bound of a reference parameter does not make a conditional
// store safe to execute outside its guard.

// { dg-do compile }
// { dg-options "-O2 -fdelete-null-pointer-checks -fno-tree-cselim -fno-ssa-phiopt -fdump-tree-lim2-details" }
// { dg-additional-options "-fallow-store-data-races" }
// { dg-skip-if "" keeps_null_pointer_checks }

struct S { int value; };

void
f (const S &s, int n, int store)
{
  for (int i = 0; i < n; ++i)
    if (store)
      const_cast<S &> (s).value = 1;
}

void
g (S &s, int n, int store)
{
  // A non-const reference can designate a const object after const_cast.
  // The path that does not store is still valid.
  for (int i = 0; i < n; ++i)
    if (store)
      s.value = 1;
}

// { dg-final { scan-tree-dump-times "Memory reference \[^\n\r]*value" 2 "lim2" } }
// { dg-final { scan-tree-dump-not "Executing store motion of \[^\n\r]*value" "lim2" } }
