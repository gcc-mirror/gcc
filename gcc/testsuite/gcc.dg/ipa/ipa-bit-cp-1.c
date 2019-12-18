/* { dg-do run } */
/* { dg-options "-O2 -w -fipa-bit-cp"  } */
static int
__attribute__ ((noinline))
test (int a)
{
   if (!(a&2))
     link_error ();
}
main()
{
  test (2);
  test (3);
  test (6);
  return 0;
}
