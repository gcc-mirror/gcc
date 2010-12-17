/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

int j;

void
__attribute__((noinline))
foo (int n)
{
  int npairs, i;
  npairs = n - (-__INT_MAX__ - 1);

  if (npairs > 0)
    for (i = 0; i < npairs; i++)
      j++;
}

int
main ()
{
  foo (5 - __INT_MAX__ - 1);

  if (j != 5)
    abort ();

  return 0;
}
