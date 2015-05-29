// { dg-do compile }
// { dg-options "-fdump-tree-gimple" }

void foo (int a, int b, int c, int d)
{
  int v[4] = {a, b, c, d};
}

// { dg-final { scan-tree-dump-not "v = {}"  "gimple" } }
