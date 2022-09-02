// { dg-do compile }
// { dg-options "-O2 -ffinite-math-only -fdump-tree-evrp" }

void bar(float);

void funk(int cond)
{
  float x;

  if (cond)
    x = __builtin_nan ("");
  else
    x = 1.24;

  bar(x);
}

// { dg-final { scan-tree-dump-times "bar \\(1.24" 1 "evrp" } }
