/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

/* Value-numbering used to not terminate, oscillating between CSEing
   the c0/c1 PHI cycle to the a0/a1 and the b0/b1 PHI cycle.  */

int f()
{
  int a0 = 0, a1 = 0;
  int b0 = 0, b1 = 0;
  int c0 = 0, c1 = 0;
  for (int k = 0; k < 100; ++k)
    {
      int na = a0 + 1, nb = b0 + 1, nc = c0 + 1;
      a0 = a1; a1 = na;
      b0 = b1; b1 = nb;
      c0 = c1; c1 = nc;
    }
  return a0 + b0 + c0;
}

/* All three PHI cycles are equivalent, k, a1 and one of a0, b0 and c0 remain,
   and we multiply that by 3.  */
/* { dg-final { scan-tree-dump-times " = PHI" 3 "fre1" } } */
/* { dg-final { scan-tree-dump " \\\* 3;" "fre1" } } */
