/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

int f(int i)
{
  if (i < 0)
    return i/10+ i;
  return i/10 + i;
}

/* Hoisting of i/10 + i should make the code straight-line
   with one division.  */

/* { dg-final { scan-tree-dump-times "goto" 0 "pre" } } */
/* { dg-final { scan-tree-dump-times " / 10;" 1 "pre" } } */
