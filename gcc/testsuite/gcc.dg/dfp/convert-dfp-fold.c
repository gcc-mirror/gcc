/* { dg-options "-O2" } */

/* N1150 5.2 Conversions among decimal floating types and between
   decimal floating types and generic floating types.
   C99 6.3.1.5(3) New.  */

extern void link_error ();

int
main ()
{
  _Decimal32 d32;
  _Decimal64 d64;
  _Decimal128 d128;

  /* Conversions to larger types.  */
  d32 = 123.4df;
  d64 = d32;
  if (d64 != 123.4dd)
    link_error ();
  d128 = d32;
  if (d128 != 123.4dl)
    link_error ();
  d64 = 345.678dd;
  d128 = d64;
  if (d128 != 345.678dl)
    link_error ();

  /* Conversions to smaller types for which the value fits.  */
  d64 = 3456.789dd;
  d32 = d64;
  if (d32 != 3456.789df)
    link_error ();
  d128 = 123.4567dl;
  d32 = d128;
  if (d32 != 123.4567dl)
    link_error ();

  d128 = 1234567890.123456dl;
  d64 = d128;
  if (d64 != 1234567890.123456dd)
    link_error ();

  return 0;
}
