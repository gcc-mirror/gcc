/* Test omitted parameter names in C23.  Execution test.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

extern void abort (void);
extern void exit (int);

void
f (int a, int [++a], int b)
{
  /* Verify array size expression of unnamed parameter is processed as
     expected.  */
  if (a != 2 || b != 3)
    abort ();
}

int
main (void)
{
  int t[2];
  f (1, t, 3);
  exit (0);
}
