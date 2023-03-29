/* Test storage duration of compound literals in parameter lists for C2x.  */
/* { dg-do run } */
/* { dg-options "-std=c2x -pedantic-errors" } */

extern void abort (void);
extern void exit (int);

int x;

void f (int a[(int) { x }]);

int *q;

int
fp (int *p)
{
  q = p;
  return 1;
}

void
g (int a, int b[fp ((int [2]) { a, a + 2 })])
{
  if (q[0] != a || q[1] != a + 2)
    abort ();
}

int
main (void)
{
  int t[1] = { 0 };
  g (1, t);
  g (2, t);
  exit (0);
}
