/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (int a, int b)
{
  int x = a - 4;
  if (a < 4)
    return x;
  else
    return 0;
}

/* { dg-final { scan-assembler-times "subs\\tw\[0-9\]+, w\[0-9\]+, \[#\]?4" 1 } } */
/* { dg-final { scan-assembler-not "cmp\\tw\[0-9\]+, w\[0-9\]+" } } */
