// { dg-do compile }
// { dg-options "-Os -fsanitize=signed-integer-overflow -fdump-tree-evrp" }

// Test that .UBSAN_CHECK_SUB(y, x) is treated as y-x for range
// purposes, where X and Y are related to each other.
//
// This effectively checks that range relationals work with builtins.

void unreachable();

int foobar(int x, int y)
{
  if (x < y)
    {
      int z = y - x;
      if (z == 0)
        unreachable();
      return z;
    }
  return 5;
}

// { dg-final { scan-tree-dump-not "unreachable" "evrp" } }
