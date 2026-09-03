// A reference parameter proves that an in-bounds load is safe.  It does not
// prove that the referenced object is writable.  Keep the store conditional.

// { dg-do compile }
// { dg-options "-O2 -fdelete-null-pointer-checks -ftree-cselim -fdump-tree-phiopt1-details" }
// { dg-additional-options "-fallow-store-data-races" }
// { dg-skip-if "" keeps_null_pointer_checks }

void
f (const int &a, int b, int d)
{
  int &c = const_cast<int &> (a);
  int p = a;
  if (b)
    c = d | p;
}

void
g (int &a, int b, int d)
{
  // A non-const reference can designate a const object after const_cast.
  // The path that does not store is still valid.
  int p = a;
  if (b)
    a = d | p;
}

// { dg-final { scan-tree-dump-times "if \\(b_" 2 "phiopt1" } }
// { dg-final { scan-tree-dump-not "Conditional store replacement happened" "phiopt1" } }
