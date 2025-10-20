/* PR tree-optimization/95699 */
/* PR tree-optimization/101024 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-phiopt1" } */
unsigned long long
f2 (unsigned long long x)
{
  if (x < 0x8000000000000000ULL)
    x = 0x8000000000000000ULL;
  else
    {
        if (x >= 0x8000000000000023ULL)
          x = 0x8000000000000023ULL;
    }
  return x;
}
unsigned long long
f1 (unsigned long long x)
{
  if (x >= 100)
    {
        if (x >= 0x8000000000000000ULL)
          x = 0x8000000000000000ULL;
    }
  else
    x = 100;
  return x;
}
/* f2: */
/* { dg-final { scan-tree-dump "MIN_EXPR <\[^>\n\r]*9223372036854775843>" "optimized" } } */
/* { dg-final { scan-tree-dump "MAX_EXPR <\[^>\n\r]*9223372036854775808>" "optimized" } } */
/* { dg-final { scan-tree-dump "MIN_EXPR <\[^>\n\r]*9223372036854775843>" "phiopt1" } } */
/* { dg-final { scan-tree-dump "MAX_EXPR <\[^>\n\r]*9223372036854775808>" "phiopt1" } } */

/* f1: */
/* { dg-final { scan-tree-dump "MIN_EXPR <\[^>\n\r]*9223372036854775808>" "optimized" } } */
/* { dg-final { scan-tree-dump "MAX_EXPR <\[^>\n\r]*100>" "optimized" } } */
/* { dg-final { scan-tree-dump "MIN_EXPR <\[^>\n\r]*9223372036854775808>" "phiopt1" } } */
/* { dg-final { scan-tree-dump "MAX_EXPR <\[^>\n\r]*100>" "phiopt1" } } */
