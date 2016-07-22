/* PR37997 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-details -fno-code-hoisting" } */

int foo (int i, int b, int result)
{
  int mask;
  if (b)
    mask = -2;
  else
    mask = 0;
  result = i + 1;
  result = result & mask;
  return result;
}

/* We should insert i + 1 into the if (b) path as well as the simplified
   i + 1 & -2 expression.  And do replacement with two PHI temps.  */
/* With hoisting enabled we'd hoist i + 1 to before the if, retaining
   only one PHI node.  */

/* { dg-final { scan-tree-dump-times "with prephitmp" 2 "pre" } } */
