// { dg-do compile }
// { dg-options "-O2 -fdump-tree-evrp" }

// Test that the only thing we know about the signbit about negative number is
// that it's not 0.

void link_error ();

int num;

void func(float x)
{
  if (x < -5.0)
    {
      num = __builtin_signbit (x);

      // We may not know the exact signbit, but we know it's not 0.
      if (!__builtin_signbit (x))
	link_error ();
    }
}

// { dg-final { scan-tree-dump-not "num = \[-0-9\];" "evrp" } }
// { dg-final { scan-tree-dump-not "link_error" "evrp" } }
