/* PR37997 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-details" } */

int foo (int i, int b, int result)
{
  int mask;
  if (b)
    mask = -1;
  else
    mask = 0;
  result = i + 1;
  result = result & mask;
  return result;
}

/* We should insert i + 1 into the if (b) path as well as the simplified
   i + 1 & -1 expression.  And do replacement with two PHI temps.  */

/* { dg-final { scan-tree-dump-times "with prephitmp" 2 "pre" } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
