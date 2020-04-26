/* { dg-do compile } */
/* { dg-options "-O1 -fvariable-expansion-in-unroller -funroll-loops -fcompare-debug" } */

int sum(int *buf, int len)
{
  int s = 0;
  while (--len > 0) s += *buf++;
  return s;
}
