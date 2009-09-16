/* { dg-do compile } */
/* { dg-options "-Wall" } */

/* N1312 7.1.1: The FLOAT_CONST_DECIMAL64 pragma.
   C99 6.4.4.2a (New).

   Check that when pragma FLOAT_CONST_DECIMAL64 is in effect so that
   unsuffixed constants are _Decimal64, invalid types are still reported
   as invalid.  */

double
f1 (void)
{
#pragma STDC FLOAT_CONST_DECIMAL64 OFF
  double a = 0x1.0p1;
  double b = 1.0i;

  return a + b;
}

double
f2 (void)
{
#pragma STDC FLOAT_CONST_DECIMAL64 OFF
  double a = 0x1.0p1dd;		/* { dg-error "with hex" } */
  double b = 1.0idd;		/* { dg-error "invalid suffix" } */

  return a + b;
}

double
f3 (void)
{
#pragma STDC FLOAT_CONST_DECIMAL64 ON
  double a = 0x1.0p1;	/* Hex constant is not affected by pragma.  */
  double b = 1.0i;	/* Imaginary constant is not affected by pragma.  */

  return a + b;
}
