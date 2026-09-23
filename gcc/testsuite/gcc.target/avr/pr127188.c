/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

extern int foo (int);

int bar (int a, int b)
{
  int q;
  if (a < 0)
    q = 0;
  else
    q = 1;
  int c = foo (b);
  if (q != 0)
    c = foo (c);
  int d = foo (c);
  return d;
}
