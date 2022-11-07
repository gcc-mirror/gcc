// { dg-do compile }
// { dg-options "-O2 -fdump-tree-evrp" }

int num;

void func(float x)
{
  if (x > 5.0)
    num = __builtin_signbit (x);
}

// { dg-final { scan-tree-dump-times "num = 0;" 1 "evrp" } }
