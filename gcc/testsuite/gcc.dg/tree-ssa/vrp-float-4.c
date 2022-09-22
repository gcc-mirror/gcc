// { dg-do compile }
// { dg-options "-O2 -fdisable-tree-ethread -fdump-tree-evrp" }

void link_error ();

void
foo (double x, double y)
{
  if (x > y)
    {
      if (__builtin_isnan (x) || __builtin_isnan (y))
        link_error ();
    }
}

// { dg-final { scan-tree-dump-not "link_error" "evrp" } }
