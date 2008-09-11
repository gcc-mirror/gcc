/* Copyright (C) 2003, 2006  Free Software Foundation.

   Verify that built-in math function constant folding of functions
   with one constant argument is correctly performed by the compiler.

   Written by Roger Sayle, 30th March 2003.  */

/* { dg-do run } */
/* { dg-options "-O2 -ffast-math" } */

extern void abort(void);
extern double pow(double, double);
extern double sqrt(double);
extern double cbrt(double);

void test(double x)
{
  if (pow(x,-1.0) != 1.0/x)
    abort ();

  if (pow(x,2.0) != x*x)
    abort ();

  if (pow(x,-2.0) != 1.0/(x*x))
    abort ();

  if (pow(x,0.5) != sqrt(x))
    abort ();

#ifdef HAVE_C99_RUNTIME
  if (pow(x,1.0/3.0) != cbrt(x))
    abort ();
#endif
}

int main()
{
  test (1.0);
  test (2.0);
  return 0;
}

