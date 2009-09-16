/* { dg-options "-O2 -w" } */

/* N1150 5.1 Conversions from decimal float to integer.  */

/* Test decimal float to integer conversions for values at the limit of
   what will fit into the destination type.  This assumes 32-bit int and
   64-bit long long (there's a check for that below).  This version tests
   conversions during compilation.  */

extern void link_error (void);

void
doit ()
{
  _Decimal32 d32;
  _Decimal64 d64;
  _Decimal128 d128;
  int si;
  unsigned int ui;
  long long sll;
  unsigned long long ull;

  /* _Decimal32 to int.  */

  d32 = 2147483.E3DF;
  si = d32;
  if (si != 2147483000)
    link_error ();

  d32 = -2147483.E3DF;
  si = d32;
  if (si != -2147483000)
    link_error ();

  /* _Decimal32 to unsigned int.  */

  d32 = 4.294967E9DF;
  ui = d32;
  if (ui != 4294967000U)
    link_error ();

  /* _Decimal32 to long long.  */

  d32 = 922.3372E16DF;
  sll = d32;
  if (sll != 9223372000000000000LL)
    link_error ();

  d32 = -92233.72E14DF;
  sll = d32;
  if (sll != -9223372000000000000LL)
    link_error ();

  /* _Decimal32 to unsigned long long.  */

  d32 = 0.1844674E20DF;
  ull = d32;
  if (ull != 18446740000000000000ULL)
    link_error ();

  /* _Decimal64 to int.  */

  d64 = 2.147483647E9DD;
  si = d64;
  if (si != 2147483647)
    link_error ();

  d64 = -2147483648.DD;
  si = d64;
  if (si != -2147483648)
    link_error ();

  /* _Decimal64 to unsigned int.  */

  d64 = 42949.67295E5DD;
  ui = d64;
  if (ui != 4294967295)
    link_error ();

  /* _Decimal64 to long long.  */

  d64 = 9.223372036854775E18DD;
  sll = d64;
  if (sll != 9223372036854775000LL)
    link_error (); 

  d64 = -92233720.36854775E11DD;
  sll = d64;
  if (sll != -9223372036854775000LL)
    link_error ();

  /* _Decimal64 to unsigned long long.  */
  d64 = 1844674407370955.E4DD;
  ull = d64;
  if (ull != 18446744073709550000ULL)
    link_error ();

  /* _Decimal128 to int.  */

  d128 = 2.147483647E9DL;
  si = d128;
  if (si != 2147483647)
    link_error ();

  d128 = -2147483648.DL;
  si = d128;
  if (si != -2147483648)
    link_error ();

  /* _Decimal128 to unsigned int.  */

  d128 = 4294.967295E6DL;
  ui = d128;
  if (ui != 4294967295)
    link_error ();

  /* _Decimal128 to long long.  */

  d128 = 9223372036854775807.DL;
  sll = d128;
  if (sll != 9223372036854775807LL)
    link_error (); 

  d128 = -9.223372036854775808E19DL;
  sll = d128;
  if (sll != -9223372036854775807LL - 1LL)
    link_error ();

  /* _Decimal128 to unsigned long long.  */
  d128 = 18446744073709551615.DL;
  ull = d128;
  if (ull != 18446744073709551615ULL)
    link_error ();
}

int
main ()
{
  /* This test assumes 32-bit int and 64-bit long long.  */

  if (sizeof (int) != 4 || sizeof (long long) != 8)
    return 0;

  doit ();
  return 0;
}
