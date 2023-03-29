// { dg-do compile }
// { dg-options "-O2 -fno-thread-jumps -fdisable-tree-fre1 -fdump-tree-evrp" }

void link_error ();

float
foo (float x)
{
  if (__builtin_isnan (x))
    {
      if (!__builtin_isnan (x))
	link_error ();
    }
}

// { dg-final { scan-tree-dump-not "link_error" "evrp" } }
