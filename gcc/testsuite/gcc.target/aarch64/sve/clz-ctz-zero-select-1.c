/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve -fdump-tree-optimized" } */

/* The vectorizer counts the signed Q[I] as unsigned.  The select of
   the result for zero must still fold into the count.  */

void
f1 (long long *p, long long *q)
{
  for (int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_ctzll (q[i]) : 64;
}

void
f2 (long long *p, long long *q)
{
  for (int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_clzll (q[i]) : 64;
}

void
f3 (int *p, int *q)
{
  for (int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_ctz (q[i]) : 32;
}

void
f4 (int *p, int *q)
{
  for (int i = 0; i < 2048; ++i)
    p[i] = q[i] ? __builtin_clz (q[i]) : 32;
}

/* { dg-final { scan-tree-dump-times {\.CTZ \(vect[^,]*, 64\)} 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times {\.CLZ \(vect[^,]*, 64\)} 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times {\.CTZ \(vect[^,]*, 32\)} 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times {\.CLZ \(vect[^,]*, 32\)} 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not {\.VCOND_MASK} "optimized" } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
