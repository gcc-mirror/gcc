// { dg-do compile }
// { dg-options "-O2 -fno-thread-jumps -fdump-tree-evrp" }

void link_error ();

void
foo (double x, double y)
{
  if (x > y && __builtin_signbit (y) == 0)
    {
      // y == +INF is impossible.
      if (__builtin_isinf (y))
        link_error ();
    }
}

// { dg-final { scan-tree-dump-not "link_error" "evrp" { xfail s390*-*-* } } } xfail: PR114678
