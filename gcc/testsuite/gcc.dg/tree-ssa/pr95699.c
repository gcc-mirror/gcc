/* PR tree-optimization/95699 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "MAX_EXPR <\[^>\n\r]*9223372036854775807\[^>\n\r]*>" "optimized" } } */
/* { dg-final { scan-tree-dump "MAX_EXPR <\[^>\n\r]*9223372036854775808\[^>\n\r]*>" "optimized" } } */
/* { dg-final { scan-tree-dump "MIN_EXPR <\[^>\n\r]*9223372036854775807\[^>\n\r]*>" "optimized" } } */
/* { dg-final { scan-tree-dump "MIN_EXPR <\[^>\n\r]*9223372036854775808\[^>\n\r]*>" "optimized" } } */

unsigned long long
f1 (unsigned long long x)
{
  if (x < 0x7fffffffffffffffULL)
    x = 0x7fffffffffffffffULL;
  return x;
}

unsigned long long
f2 (unsigned long long x)
{
  if (x < 0x8000000000000000ULL)
    x = 0x8000000000000000ULL;
  return x;
}

unsigned long long
f3 (unsigned long long x)
{
  if (x >= 0x7fffffffffffffffULL)
    x = 0x7fffffffffffffffULL;
  return x;
}

unsigned long long
f4 (unsigned long long x)
{
  if (x >= 0x8000000000000000ULL)
    x = 0x8000000000000000ULL;
  return x;
}
