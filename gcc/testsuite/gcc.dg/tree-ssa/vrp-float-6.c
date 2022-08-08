// { dg-do compile }
// { dg-options "-O2 -fdisable-tree-ethread -fdump-tree-evrp" }

void bar ();

void
foo (double x, double y)
{
      if (x > y)
	;
      else if (!__builtin_isnan (x) && !__builtin_isnan (y))
	{
	  // If x and y are not NAN, the x <= y relationship holds, and the
	  // following conditional can be folded away.
	  if (x <= y)
	    bar ();
	}
}

// { dg-final { scan-tree-dump-times "Folding predicate x_.* <= y_.* to 1" 1 "evrp" } }
