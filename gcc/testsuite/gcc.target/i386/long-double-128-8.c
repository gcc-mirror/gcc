/* { dg-do run } */
/* { dg-options "-O0 -mlong-double-64 -mfpmath=387" } */

int
main ()
{
  __float128 a = -0.23456789;
  if ((double) a >= 0)
    __builtin_abort ();
  return 0;
}
