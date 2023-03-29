/* { dg-do run } */
/* { dg-options "-O1 -ftree-vrp" } */

int a, c, d;
int
main()
{
  long e = 1;
  int f = a = 1;
L1:
  if (a)
    a = 2;
  int h = e = ~e;
  c = -1;
  if (e >= a)
    goto L2;
  if (-1 > a)
    goto L1;
  if (a)
    f = -1;
L2:
  d = (-f + d) & h;
  if (d)
    __builtin_abort();
  return 0;
}
