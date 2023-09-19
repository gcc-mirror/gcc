// { dg-do compile }
// { dg-options "-O2 -fdisable-tree-fre1 -fdump-tree-evrp" }

void link_error ();
void func ();

void foo1 (float x, float y)
{
   if (x != y)
     if (x == y)
       link_error();
}

void foo2 (float a, float b)
{
  if (a != b)
    // This conditional should be folded away.
    if (a != b)
      func ();
}

// { dg-final { scan-tree-dump-not "link_error" "evrp" } }
// { dg-final { scan-tree-dump-times "if \\(a_2\\(D\\) != b_3\\(D\\)" 1 "evrp" } }
