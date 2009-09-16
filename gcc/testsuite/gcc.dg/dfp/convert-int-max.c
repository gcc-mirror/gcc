/* { dg-options "-O0 -w" } */

/* N1150 5.1 Conversions from decimal float to integer.  */

/* Test decimal float to integer conversions for values at the limit of
   what will fit into the destination type.  This assumes 32-bit int and
   64-bit long long (there's a check for that below).  */

#include "dfp-dbg.h"

volatile _Decimal32 d32;
volatile _Decimal64 d64;
volatile _Decimal128 d128;
volatile int si;
volatile unsigned int ui;
volatile long long sll;
volatile unsigned long long ull;

void
doit ()
{
  /* _Decimal32 to int.  */

  d32 = 2147483.E3DF;
  si = d32;
  if (si != 2147483000)
    FAILURE

  d32 = -2147483.E3DF;
  si = d32;
  if (si != -2147483000)
    FAILURE

  /* _Decimal32 to unsigned int.  */

  d32 = 4.294967E9DF;
  ui = d32;
  if (ui != 4294967000U)
    FAILURE

  /* _Decimal32 to long long.  */

  d32 = 922.3372E16DF;
  sll = d32;
  if (sll != 9223372000000000000LL)
    FAILURE

  d32 = -92233.72E14DF;
  sll = d32;
  if (sll != -9223372000000000000LL)
    FAILURE

  /* _Decimal32 to unsigned long long.  */

  d32 = .1844674E20DF;
  ull = d32;
  if (ull != 18446740000000000000ULL)
    FAILURE

  /* _Decimal64 to int.  */

  d64 = 2.147483647E9DD;
  si = d64;
  if (si != 2147483647)
    FAILURE

  d64 = -2147483648.DD;
  si = d64;
  if (si != -2147483648)
    FAILURE

  /* _Decimal64 to unsigned int.  */

  d64 = 42949.67295E5DD;
  ui = d64;
  if (ui != 4294967295)
    FAILURE

  /* _Decimal64 to long long.  */

  d64 = 9.223372036854775E18DD;
  sll = d64;
  if (sll != 9223372036854775000LL)
    FAILURE 

  d64 = -92233720.36854775E11DD;
  sll = d64;
  if (sll != -9223372036854775000LL)
    FAILURE

  /* _Decimal64 to unsigned long long.  */
  d64 = 1844674407370955.E4DD;
  ull = d64;
  if (ull != 18446744073709550000ULL)
    FAILURE

  /* _Decimal128 to int.  */

  d128 = 2.147483647E9DL;
  si = d128;
  if (si != 2147483647)
    FAILURE

  d128 = -2147483648.DL;
  si = d128;
  if (si != -2147483648)
    FAILURE

  /* _Decimal128 to unsigned int.  */

  d128 = 4294.967295E6DL;
  ui = d128;
  if (ui != 4294967295)
    FAILURE

  /* _Decimal128 to long long.  */

  d128 = 9223372036854775807.DL;
  sll = d128;
  if (sll != 9223372036854775807LL)
    FAILURE 

  d128 = -9.223372036854775808E19DL;
  sll = d128;
  if (sll != -9223372036854775807LL - 1LL)
    FAILURE

  /* _Decimal128 to unsigned long long.  */
  d128 = 18446744073709551615.DL;
  ull = d128;
  if (ull != 18446744073709551615ULL)
    FAILURE
}

int
main ()
{
  /* This test assumes 32-bit int and 64-bit long long.  */

  if (sizeof (int) != 4 || sizeof (long long) != 8)
    return 0;

  doit ();

  FINISH
}
