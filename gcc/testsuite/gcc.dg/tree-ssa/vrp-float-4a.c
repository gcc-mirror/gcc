// { dg-do compile }
// { dg-options "-O2 -fno-thread-jumps -fdisable-tree-fre1 -fdump-tree-evrp" }

void link_error ();
void bar ();

float
foo (float x)
{
  if (x == x)
    {
      bar ();
    }
  else
    {
      // The false side of x == x implies NAN, so we should be able to
      // fold this.
      if (!__builtin_isnan (x))
	link_error ();
    }
}

// { dg-final { scan-tree-dump-not "link_error" "evrp" } }
