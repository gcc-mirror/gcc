/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiopt2" } */

int f0(int i)
{
  if (i == 0) return 0;
  return i/10;
}
int f1(int i)
{
  return i?i/10:0;
}

int f2(int i)
{
  if (i == 0) return 0;
  return i%10;
}
int f3(int i)
{
  return i?i%10:0;
}

int f4(int i)
{
  if (i == 0) return 0;
  return i<<10;
}
int f5(int i)
{
  return i?i<<10:0;
}

/* We should if-convert all functions to carry out the operation
   unconditionally.  */
/* { dg-final { scan-tree-dump-not "= PHI" "phiopt2" } } */
