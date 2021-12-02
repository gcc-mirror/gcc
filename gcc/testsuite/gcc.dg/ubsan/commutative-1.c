/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined -fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-flto" } { "" } } */

int res[2];

void
f1 (int x, int y)
{
  res[0] = x + y;
  res[1] = y + x;
}

void
f2 (int x, int y)
{
  res[0] = x - y;
  res[1] = y - x;
}

void
f3 (int x, int y)
{
  res[0] = x * y;
  res[1] = y * x;
}

/* { dg-final { scan-tree-dump-times {\.UBSAN_CHECK_ADD} 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times {\.UBSAN_CHECK_SUB} 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times {\.UBSAN_CHECK_MUL} 1 "optimized" } } */
