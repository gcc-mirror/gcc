/* { dg-options "-O2" } */
/* { dg-do run } */

static void __attribute__((noinline))
foo (double *a, double x)
{
  *a = x;
}

static double  __attribute__((noinline))
f_c1 (int m, double *a)
{
  foo (a, m);
  return *a;
}

int
main (){
  double data;
  double ret = 0 ;

  if ((ret = f_c1 (2, &data)) != 2)
    {
      __builtin_abort ();
    }
  return 0;
}
