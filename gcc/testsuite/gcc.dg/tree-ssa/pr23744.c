/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp -fdisable-tree-evrp -fdump-tree-vrp1-details" } */

void h (void);

int g (int i, int j)
{
  int t = 0;
  int i1;

  if (i == j)
    t = 3;
  for (i1 = 0; i1 < 10000; i1++) h();
  if (t != 5)
    return 0;
  else
    return 1;
}

/* { dg-final { scan-tree-dump-times "gimple_simplified" 1 "vrp1" } } */
