/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve -fdump-tree-optimized" } */

void
f1 (double x[][4]) 
{
  for (int i = 0; i < 4; ++i)
    for (int j = 0; j < 4; ++j)
      x[i][j] = 0;
}

void
f2 (double x[][4], double y)
{
  for (int i = 0; i < 4; ++i)
    for (int j = 0; j < 4; ++j)
      x[i][j] = y;
}

/* { dg-final { scan-tree-dump-not "VEC_PERM_EXPR" "optimized"} } */
