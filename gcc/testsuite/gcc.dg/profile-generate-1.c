/* Bug 16325.  */
/* { dg-options "-O -fprofile-generate" } */

extern void abort (void);

int *p1;
int *p2;
int *p3;

int ga = 100;

int
sub (int i, int j)
{
  int k;
  int l;
  int m;
  int n;
  p1 = &k;
  p2 = &l;
  p3 = &m;
  k = 20;
  l = 30;
  m = 40;
  n = i / j;
  return n + ga;
}

int
main(void)
{
  if (sub (99, 33) != 103)
    abort ();
  return 0;
}
