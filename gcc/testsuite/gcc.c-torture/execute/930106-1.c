/* { dg-add-options stack_size } */
/* { dg-additional-options "-std=gnu89" } */

#if defined (STACK_SIZE)
#define DUMMY_SIZE 9
#else
#define DUMMY_SIZE 399999
#endif

double g()
{
  return 1.0;
}

f()
{
  char dummy[DUMMY_SIZE];
  double f1, f2, f3;
  f1 = g();
  f2 = g();
  f3 = g();
  return f1 + f2 + f3;
}

main()
{
  if (f() != 3.0)
    abort();
  exit(0);
}
