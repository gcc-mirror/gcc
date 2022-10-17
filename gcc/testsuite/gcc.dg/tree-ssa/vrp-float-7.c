// { dg-do compile }
// { dg-options "-O2 -fno-tree-forwprop -fno-tree-ccp -fno-tree-fre -fdump-tree-evrp" }

extern void link_error ();

void
foo ()
{
  float z = 0.0;
  if (__builtin_isnan (z))
    link_error ();
}

// { dg-final { scan-tree-dump-not "link_error" "evrp" } }
