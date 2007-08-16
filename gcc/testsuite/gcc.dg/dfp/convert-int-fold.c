/* { dg-options "-std=gnu99 -O2" } */

/* N1150 5.1 Conversion between decimal floating integer.
   C99 6.3.1.4(1a) New.
   These should all be folded at compile time.  */

extern void link_error (void);

int
main ()
{
  _Decimal32 d32;
  _Decimal64 d64;
  _Decimal128 d128;
  unsigned int ui;
  unsigned long ul;
  unsigned long long ull;
  int si;
  long sl;
  long long sll;
  _Bool b;

  /* C99 Section 6.7.2 Type specifiers.  Type _Bool is 
     mentioned in this section.  Conversions between 
     _Bool and DFP types.  */

  /* Decimal float to unsigned integer.  */
  d32 = 456.789df;
  d64 = 23.456789dd;
  d128 = 1234.5678dl;

  ui = d32;
  if (ui != 456U)
    link_error ();
  ul = d32;
  if (ul != 456UL)
    link_error ();
  ull = d32;
  if (ull != 456ULL)
    link_error ();

  ui = d64;
  if (ui != 23U)
    link_error ();
  ul = d64;
  if (ul != 23UL)
    link_error ();
  ull = d64;
  if (ull != 23ULL)
    link_error ();

  ui = d128;
  if (ui != 1234U)
    link_error ();
  ul = d128;
  if (ul != 1234UL)
    link_error ();
  ull = d128;
  if (ull != 1234ULL)
    link_error ();

  /* Decimal float to signed integer.  */

  /* Decimal float to _Bool.  */
  d32 = 1.23df;
  d64 = -3.4dd;
  d128 = 0.00003dl;

  b = d32;
  if (!b)
    link_error ();
  b = d64;
  if (!b)
    link_error ();
  b = d128;
  if (!b)
    link_error ();

  /* Unsigned integer to decimal float.  */
  ui = 987U;
  ul = 345678UL;
  ull = 1234567ULL;

  d32 = ui;
  if (d32 != 987.0df)
    link_error ();
  d32 = ul;
  if (d32 != 345678.0df)
    link_error ();
  d32 = ull;
  if (d32 != 1234567.df)
    link_error ();

  d64 = ui;
  if (d64 != 987.0dd)
    link_error ();
  d64 = ul;
  if (d64 != 345678.0dd)
    link_error ();
  d64 = ull;
  if (d64 != 1234567.dd)
    link_error ();

  d128 = ui;
  if (d128 != 987.0dl)
    link_error ();
  d128 = ul;
  if (d128 != 345678.0dl)
    link_error ();
  d128 = ull;
  if (d128 != 1234567.dl)
    link_error ();

  /* Signed integer to decimal float.  */
  si = -987;
  sl = -345678;
  sll = -1234567;

  d32 = si;
  if (d32 != -987.0df)
    link_error ();
  d32 = sl;
  if (d32 != -345678.0df)
    link_error ();
  d32 = sll;
  if (d32 != -1234567.df)
    link_error ();

  d64 = si;
  if (d64 != -987.0dd)
    link_error ();
  d64 = sl;
  if (d64 != -345678.0dd)
    link_error ();
  d64 = sll;
  if (d64 != -1234567.dd)
    link_error ();

  d128 = si;
  if (d128 != -987.0dl)
    link_error ();
  d128 = sl;
  if (d128 != -345678.0dl)
    link_error ();
  d128 = sll;
  if (d128 != -1234567.dl)
    link_error ();

  /* _Bool to decimal float.  */
  d32 = 0.0DF;
  d64 = 0.0DD;
  d128 = 0.0DL;
  
  b = d32;
  if (b)
    link_error ();
  b = d64;
  if (b)
    link_error ();
  b = d128;
  if (b)
    link_error ();

  return 0;
}
