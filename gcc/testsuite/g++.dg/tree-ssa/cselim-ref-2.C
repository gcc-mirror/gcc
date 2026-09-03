// Check the full conditional store replacement after disabling the early
// phi optimization.

// { dg-do compile }
// { dg-options "-O2 -fdelete-null-pointer-checks -ftree-cselim -fno-ssa-phiopt -fdump-tree-cselim-details" }
// { dg-additional-options "-fallow-store-data-races" }
// { dg-skip-if "" keeps_null_pointer_checks }

void
f (const int &a, int b, int d)
{
  if (b)
    const_cast<int &> (a) = d;
}

void
g (int &a, int b, int d)
{
  // A non-const reference can designate a const object after const_cast.
  // The path that does not store is still valid.
  if (b)
    a = d;
}

// { dg-final { scan-tree-dump-times "if \\(b_" 2 "cselim" } }
// { dg-final { scan-tree-dump-not "Conditional store replacement happened" "cselim" } }
