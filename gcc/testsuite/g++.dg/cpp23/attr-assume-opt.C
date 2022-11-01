// P1774R8 - Portable assumptions
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-vrp2" }
// Test the we can optimize based on conditions in assume.

int
f1 (unsigned x, unsigned y, unsigned z)
{
  [[assume (x == 2 && y < 3 && z < 20)]];
  unsigned q = x + y + z;
  if (q > 23)
    return 0;
  return 1;
}


int
f2 (int x, int y, int z)
{
  [[assume (x+12 == 14 && y >= 0 && y + 10 < 13 && z + 4 >= 4 && z - 2 < 18)]];
  unsigned q = x + y + z;
  if (q*2 > 46)
    return 0;
  return 1;
}

int
f3 (int x, int y, int z)
{
  [[assume (x + 12 == 14 && z / 2 > 0)]];
  [[assume (y >= 0 && z - 2 < 18)]];
  [[assume (y + 10 < 13 && z + 4 >= 2)]];
  int q = x + y + z;
  if (q * 2 > 46)
    return 0;
  if (z < 0)
    return 0;
  return 1;
}

/* { dg-final { scan-tree-dump-times "return 0" 0 "vrp2" } } */
/* { dg-final { scan-tree-dump-times "return 1" 3 "vrp2" } } */
