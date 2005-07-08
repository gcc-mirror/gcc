/* { dg-do compile } */
/* { dg-options "-O2" } */
int f(int i)
{
  int k = 0;
  if (i == 0)
    k = i == 0;
  return k;
}
