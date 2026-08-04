// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized" }

extern void link_error ();

void
test (double x)
{
  if (x != 3.0)
    {
      double z = x + 0.0;
      if (z == 3.0)
	link_error ();
    }
}

// { dg-final { scan-tree-dump-not "link_error" "optimized" } }
