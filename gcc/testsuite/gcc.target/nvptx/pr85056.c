/* { dg-do run } */
/* { dg-additional-sources "pr85056a.c" } */

extern void abort ();

extern int a[];

int
main ()
{
  int i, sum;

  sum = 0;
  for (i = 0; i < 10; i++)
    sum += a[i];

  if (sum != 55)
    abort ();

  return 0;
}
