/* { dg-do run } */
/* { dg-options "-O1 -funroll-loops" } */
/* { dg-add-options ieee } */

extern void exit (int);
extern void abort (void);

void
compare (double a, double b)
{
  do
    {
      double s1 = __builtin_copysign ((double) 1.0, a);
      double s2 = __builtin_copysign ((double) 1.0, b);

      if (s1 != s2)
        abort ();

      if ((__builtin_isnan (a) != 0) != (__builtin_isnan (b) != 0))
        abort ();

      if ((a != b) != (__builtin_isnan (a) != 0))
        abort ();
    } while (0);
}

int
main ()
{
  double a = 0.0;
  double b = 0.0;
  _Complex double cr = __builtin_complex (a, b);
  static _Complex double cs = __builtin_complex (0.0, 0.0);

  compare (__real__ cr, 0.0);
  compare (__imag__ cr, 0.0);
  compare (__real__ cs, 0.0);
  compare (__imag__ cs, 0.0);

  exit (0);
}
