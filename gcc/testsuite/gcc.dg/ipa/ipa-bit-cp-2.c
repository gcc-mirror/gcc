/* { dg-do run } */
/* { dg-options "-O2 -w -fipa-bit-cp"  } */
static int
__attribute__ ((noinline))
test (int __attribute__((unused)) b, int a)
{
   if (!(a&2))
     link_error ();
}

extern int __attribute__((const)) getint ();

main()
{
  test (getint(), 2);
  test (getint(), 3);
  test (getint(), 6);
  return 0;
}
