/* { dg-do compile } */
/* { dg-options "-O2" } */

int f(int a, int b)
{
  if(a<b)
    return 1;
  if(a>b)
    return -1;
  return 0;
}

/* We should optimize away the second cmp. */
/* { dg-final { scan-assembler-times "cmp\tw" 1 } } */

