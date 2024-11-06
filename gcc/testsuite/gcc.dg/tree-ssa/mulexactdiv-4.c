/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

int
f1 (int x)
{
  if (x & 15)
    __builtin_unreachable ();
  x /= 2;
  x = (unsigned short) x * 4;
  return x;
}


/* { dg-final { scan-tree-dump {<exact_div_expr, } "optimized" } } */
