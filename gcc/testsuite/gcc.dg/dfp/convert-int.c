/* { dg-options "-O0" } */

/* N1150 5.1 Conversion between decimal floating types and integer.
   C99 6.3.1.4(1a) New.  */

#include "dfp-dbg.h"

#ifdef __cplusplus
#define BOOL bool
#else
#define BOOL _Bool
#endif

_Decimal32 d32;
_Decimal64 d64;
_Decimal128 d128;
unsigned int ui;
unsigned long ul;
unsigned long long ull;
int si;
long sl;
long long sll;
BOOL b;

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
     BOOL and DFP types.  */

  /* Decimal float to unsigned integer.  */
  init_dfp_1 ();

  ui = d32;
  if (ui != 456U)
    FAILURE
  ul = d32;
  if (ul != 456UL)
    FAILURE
  ull = d32;
  if (ull != 456ULL)
    FAILURE

  ui = d64;
  if (ui != 23U)
    FAILURE
  ul = d64;
  if (ul != 23UL)
    FAILURE
  ull = d64;
  if (ull != 23ULL)
    FAILURE

  ui = d128;
  if (ui != 1234U)
    FAILURE
  ul = d128;
  if (ul != 1234UL)
    FAILURE
  ull = d128;
  if (ull != 1234ULL)
    FAILURE

  /* Decimal float to signed integer.  */

  /* Decimal float to BOOL.  */
  init_dfp_2 ();

  b = d32;
  if (!b)
    FAILURE
  b = d64;
  if (!b)
    FAILURE
  b = d128;
  if (!b)
    FAILURE

  /* Unsigned integer to decimal float.  */
  init_unsigned_int ();

  d32 = ui;
  if (d32 != 987.0df)
    FAILURE
  d32 = ul;
  if (d32 != 345678.0df)
    FAILURE
  d32 = ull;
  if (d32 != 1234567.df)
    FAILURE

  d64 = ui;
  if (d64 != 987.0dd)
    FAILURE
  d64 = ul;
  if (d64 != 345678.0dd)
    FAILURE
  d64 = ull;
  if (d64 != 1234567.dd)
    FAILURE

  d128 = ui;
  if (d128 != 987.0dl)
    FAILURE
  d128 = ul;
  if (d128 != 345678.0dl)
    FAILURE
  d128 = ull;
  if (d128 != 1234567.dl)
    FAILURE

  /* Signed integer to decimal float.  */
  init_signed_int ();

  d32 = si;
  if (d32 != -987.0df)
    FAILURE
  d32 = sl;
  if (d32 != -345678.0df)
    FAILURE
  d32 = sll;
  if (d32 != -1234567.df)
    FAILURE

  d64 = si;
  if (d64 != -987.0dd)
    FAILURE
  d64 = sl;
  if (d64 != -345678.0dd)
    FAILURE
  d64 = sll;
  if (d64 != -1234567.dd)
    FAILURE

  d128 = si;
  if (d128 != -987.0dl)
    FAILURE
  d128 = sl;
  if (d128 != -345678.0dl)
    FAILURE
  d128 = sll;
  if (d128 != -1234567.dl)
    FAILURE

  /* BOOL to decimal float.  */
  init_dfp_3 ();
  
  b = d32;
  if (b)
    FAILURE
  b = d64;
  if (b)
    FAILURE
  b = d128;
  if (b)
    FAILURE

  FINISH
}
