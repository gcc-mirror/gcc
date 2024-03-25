/* { dg-require-effective-target untyped_assembly } */
/* { dg-additional-options "-std=gnu89" } */

foo (a, b)
{
  if ((a & (1 << b)) == 0)
    return 1;
  return 0;
}

main ()
{
  printf ("%d\n", foo ());
}
