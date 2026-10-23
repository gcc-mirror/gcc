/* { dg-do run } */
/* { dg-options "-O2 -fassociative-math -fno-trapping-math -fno-signed-zeros" } */

__attribute__ ((noipa)) double
f (double a)
{
  return (a / 3) / (a / 6) + a * a;
}

int
main (void)
{
  double result = f (6);
  if (result != 38)
    __builtin_abort ();
  return 0;
}
