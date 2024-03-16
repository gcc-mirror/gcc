/* { dg-additional-options "-std=gnu89" } */

#define DD 2410065408

unsigned
foo (d)
     double d;
{
  return d;
}

#if foobar

main ()
{
#if bar
  unsigned u = DD;
  double d = (double) u;
#else
  double d = (double) DD;
#endif
  printf ("%u = %u = %lf\n", foo ((double) DD), foo (d), d);
}
#else

main ()
{
  printf ("%lf\n", (double) ((unsigned) DD));
  foo ((double) DD);
}
#endif
