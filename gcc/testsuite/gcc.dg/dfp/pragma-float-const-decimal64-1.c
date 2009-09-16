/* { dg-do compile } */
/* { dg-options "-Wall" } */

/* N1312 7.1.1: The FLOAT_CONST_DECIMAL64 pragma.
   C99 6.4.4.2a (New).

   Verify that the pragma has the expected result by using unsuffixed
   float constants as operands in expressions that would mix binary and
   decimal operands if the pragma had no effect, or the wrong effect.  */

#pragma STDC FLOAT_CONST_DECIMAL64 ON
double a = 1.0 * 2.0dd;

double
f1 (void)
{
#pragma STDC FLOAT_CONST_DECIMAL64 OFF
  double b = 2.0 * 3.0d;

  {
    double c = 3.0 * 4.0d;
    b = b + c;
  }

  {
#pragma STDC FLOAT_CONST_DECIMAL64 ON
    double d = 4.0 * 5.0dd;

    b = b + d;
  }

  {
     /* Default is OFF.  */
#pragma STDC FLOAT_CONST_DECIMAL64 DEFAULT
     double e = 5.0 * 6.0d;
     b = b + e;
  }

  return b;
}

double
f2 (void)
{
  /* Use value from outer scope, which is ON.  */
  double b = 2.0 * 3.0dd;

  {
#pragma STDC FLOAT_CONST_DECIMAL64 OFF
    double c = 3.0 * 4.0d;

    {
#pragma STDC FLOAT_CONST_DECIMAL64 ON
      double d = 4.0 * 5.0dd;

      {
#pragma STDC FLOAT_CONST_DECIMAL64 DEFAULT
	double e = 5.0 * 6.0d;

	{
#pragma STDC FLOAT_CONST_DECIMAL64 ON
	  double f = 6.0 * 7.0dd;

	  b = a + b + c + d + e + f;
	}
      }
    }
  }
  return b;
}

/* Use previous value from this scope, which is ON.  */
double f = 6.0 * 7.0dd;

double
f3 (void)
{
#pragma STDC FLOAT_CONST_DECIMAL64 OFF
  double b = 2.0 * 3.0d;

  return b + f;
}

/* Return to the state from this scope, which is ON.  */
double g = 7.0 + 8.0dd;
