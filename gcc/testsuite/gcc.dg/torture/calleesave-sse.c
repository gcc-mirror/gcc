/* { dg-do run } */
/* { dg-options "-fno-omit-frame-pointer" } */
/* { dg-require-effective-target alloca } */

#define alloca __builtin_alloca
extern void abort (void);

__attribute__ ((noinline)) static double
bar (double a, double b, double c, double d, char *h)
{
  *h = 0;
  return a * b + b + c;
}

__attribute__ ((noinline)) static int
boo (double a, double b, double c, double d)
{
  return c * b + a + b;
}

__attribute__ ((noinline)) static double
foo (double a, double b, double c, double d)
{
 int aa = boo (b, c, d, a);
 return bar (a, b, c, d, (char *) alloca (aa))
	+ bar (d, c, b, a, (char *) alloca (aa));
}

int main ()
{
  double a = 2.0, b = 3.0, c = 4.0, d = 5.0;
  double r1, r2;
  int aa;

  aa = boo (b, c, d, a);
  r1 = bar (a, b, c, d, (char *) alloca (aa))
       + bar (d, c, b, a, (char *) alloca (aa));
  r2 = foo (a, b, c, d);

  if (r1 != r2)
    abort ();
  return 0;
}

