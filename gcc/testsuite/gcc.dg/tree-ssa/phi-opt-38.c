/* { dg-options "-O2 -fno-signed-zeros -fdump-tree-phiopt" } */
int minus1(int a, int b)
{
  int c = a - b;
  if (c == 0) c = b - a;
  return c;
}
int minus2(int a, int b)
{
  int c = a - b;
  if (c != 0) c = b - a;
  return c;
}
int minus3(int a, int b)
{
  int c = a - b;
  if (c == 0) c = 0;
  else c = b - a;
  return c;
}
int minus4(int a, int b)
{
  int c;
  if (a == b) c = 0;
  else
    c = b - a;
  return c;
}
int abs0(int a, int b)
{
  int c = a - b;
  if (c <= 0) c = b - a;
  return c;
}
int negabs(int a, int b)
{
  int c = a - b;
  if (c >= 0) c = b - a;
  return c;
}

/* The above should be optimized at phiopt1 except for negabs which has to wait
  until phiopt2 as -abs is not acceptable in early phiopt.  */
/* { dg-final { scan-tree-dump-times "if" 1  "phiopt1"  } } */
/* { dg-final { scan-tree-dump-not "if" "phiopt2" } } */
