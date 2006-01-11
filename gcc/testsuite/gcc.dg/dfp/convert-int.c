/* { dg-do compile } */
/* { dg-options "-std=gnu99 -O0" } */

/* N1150 5.1 Conversion between decimal floating integer.
   C99 6.3.1.4(1a) New.  */

extern void abort (void);

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

void
init_dfp_1 (void)
{
  d32 = 456.789df;
  d64 = 23.456789dd;
  d128 = 1234.5678dl;
}
void
init_dfp_2 (void)
{
  d32 = 1.23df;
  d64 = -3.4dd;
  d128 = 0.00003dl;
}

void
init_dfp_3 (void)
{
  d32 = 0.0DF;
  d64 = 0.0DD;
  d128 = 0.0DL;
}

void
init_unsigned_int (void)
{
  ui = 987U;
  ul = 345678UL;
  ull = 1234567ULL;
}

void
init_signed_int (void)
{
  si = -987;
  sl = -345678;
  sll = -1234567;
}

int
main ()
{
  /* C99 Section 6.7.2 Type specifiers.  Type _Bool is 
     mentioned in this section.  Conversions between 
     _Bool and DFP types.  */

  /* Decimal float to unsigned integer.  */
  init_dfp_1 ();

  ui = d32;
  if (ui != 456U)
    abort ();
  ul = d32;
  if (ul != 456UL)
    abort ();
  ull = d32;
  if (ull != 456ULL)
    abort ();

  ui = d64;
  if (ui != 23U)
    abort ();
  ul = d64;
  if (ul != 23UL)
    abort ();
  ull = d64;
  if (ull != 23ULL)
    abort ();

  ui = d128;
  if (ui != 1234U)
    abort ();
  ul = d128;
  if (ul != 1234UL)
    abort ();
  ull = d128;
  if (ull != 1234ULL)
    abort ();

  /* Decimal float to signed integer.  */

  /* Decimal float to _Bool.  */
  init_dfp_2 ();

  b = d32;
  if (!b)
    abort ();
  b = d64;
  if (!b)
    abort ();
  b = d128;
  if (!b)
    abort ();

  /* Unsigned integer to decimal float.  */
  init_unsigned_int ();

  d32 = ui;
  if (d32 != 987.0df)
    abort ();
  d32 = ul;
  if (d32 != 345678.0df)
    abort ();
  d32 = ull;
  if (d32 != 1234567.df)
    abort ();

  d64 = ui;
  if (d64 != 987.0dd)
    abort ();
  d64 = ul;
  if (d64 != 345678.0dd)
    abort ();
  d64 = ull;
  if (d64 != 1234567.dd)
    abort ();

  d128 = ui;
  if (d128 != 987.0dl)
    abort ();
  d128 = ul;
  if (d128 != 345678.0dl)
    abort ();
  d128 = ull;
  if (d128 != 1234567.dl)
    abort ();

  /* Signed integer to decimal float.  */
  init_signed_int ();

  d32 = si;
  if (d32 != -987.0df)
    abort ();
  d32 = sl;
  if (d32 != -345678.0df)
    abort ();
  d32 = sll;
  if (d32 != -1234567.df)
    abort ();

  d64 = si;
  if (d64 != -987.0dd)
    abort ();
  d64 = sl;
  if (d64 != -345678.0dd)
    abort ();
  d64 = sll;
  if (d64 != -1234567.dd)
    abort ();

  d128 = si;
  if (d128 != -987.0dl)
    abort ();
  d128 = sl;
  if (d128 != -345678.0dl)
    abort ();
  d128 = sll;
  if (d128 != -1234567.dl)
    abort ();

  /* _Bool to decimal float.  */
  init_dfp_3 ();
  
  b = d32;
  if (b)
    abort ();
  b = d64;
  if (b)
    abort ();
  b = d128;
  if (b)
    abort ();

  return 0;
}
