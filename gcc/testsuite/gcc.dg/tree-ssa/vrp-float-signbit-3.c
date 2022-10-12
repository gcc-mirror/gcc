// { dg-do compile }
// { dg-options "-O2 -ffinite-math-only -fdump-tree-evrp" }

void link_error();

void foo(float x)
{
  if (__builtin_signbit (x))
    {
      if (x > 0.0)
	link_error();
    }
}

// { dg-final { scan-tree-dump-not "link_error" "evrp" } }
