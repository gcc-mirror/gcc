/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

void
f1 (int n, int b)
{
  extern void f2 (int);
  int j;

  if (b)
    n = 1;

  if (n < 1)
    __builtin_unreachable ();

  for (j = 0; j < n; j++)
    f2 (j);
}
