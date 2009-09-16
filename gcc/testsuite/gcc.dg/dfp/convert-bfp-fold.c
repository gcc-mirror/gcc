/* { dg-options "-O2" } */

/* N1150 5.2 Conversions among decimal floating types and between
   decimal floating types and generic floating types.
   C99 6.3.1.5(4) Conversions, arithmetic operands, real floating types.  */

_Decimal32 d32;
_Decimal64 d64;
_Decimal128 d128;
float sf;
double df;
long double tf;

extern void link_error (void);

int
main ()
{
  /* Conversions from decimal float to binary float. */

  /* Conversions from _Decimal32. */
  d32 = 2.0df;
  sf = d32;
  if (sf != 2.0f)
    link_error ();

  df = d32;
  if (df != 2.0)
    link_error ();

  tf = d32;
  if (tf != 2.0l)
    link_error ();

  /* Conversions from _Decimal64. */
  d64 = -7.0dd;
  sf = d64;
  if (sf != -7.0f)
    link_error ();
  
  df = d64;
  if (df != -7.0)
    link_error ();

  tf = d64;
  if (tf != -7.0l)
    link_error ();

  /* Conversions from _Decimal128. */
  d128 = 30.0dl;
  sf = d128;
  if (sf != 30.0f)
    link_error ();

  df = d128;
  if (df != 30.0)
    link_error ();

  df = d128;
  if (df != 30.0l)
    link_error ();

  /* Conversions from binary float to decimal float. */
  sf = 30.0f;
  d128 = sf;
  if (d128 != 30.0dl)
    link_error ();

  d64 = sf;
  if (d64 != 30.0dd)
    link_error ();

  d32 = sf;
  if (d32 != 30.0df)
    link_error ();

  df = -2.0;
  d128 = df;
  if (d128 != -2.0dl)
    link_error ();

  d64 = df;
  if (d64 != -2.0dd)
    link_error ();

  d32 = df;
  if (d32 != -2.0df)
    link_error ();  

  tf = -22.0l;
  d128 = tf;
  if (d128 != -22.0dl)
    link_error ();

  d64 = tf;
  if (d64 != -22.0dd)
    link_error ();

  d32 = tf;
  if (d32 != -22.0df)
    link_error ();

  /* 2**(-11) = 0.00048828125. */
  d128 = 0.000488281251dl;
  sf = d128;
  if (sf != 0.00048828125f)
    link_error ();
  /* 2**(-25) = 0.298023223876953125E-7.  */
  d128 = 2.98023223876953125E-8dl;
  df = d128;
  if (df < (2.9802322387695312e-08 - 0.00000000001)
      || df > (2.9802322387695312e-08 + 0.00000000001))
    link_error ();

  return 0;
}
