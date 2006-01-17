/* { dg-options "-std=gnu99" } */

/* C99 6.5.4 Cast operators.
   Test valid casts involving decimal float.  */

extern void abort (void);

_Decimal32 d32;
_Decimal64 d64;
_Decimal128 d128;

static float f = 2.f;
static double d = 2.l;

int
main (void)
{
  /* Casts between DFP types.  */
  d32 = 1.2df;
  d64 = 1.2dd;
  d128 = 1.2dl;

  if (d32 != (_Decimal32) d64)
    abort ();
  if (d32 != (_Decimal32) d128)
    abort ();

  if (d64 != (_Decimal64) d32)
    abort ();
  if (d64 != (_Decimal64) d128)
    abort ();

  if (d128 != (_Decimal128) d32)
    abort ();
  if (d128 != (_Decimal128) d64)
    abort ();

  /* Casts between generic and decimal floating point types.  Use a
     value that we can assume can be represented exactly in all
     representations. */
  
  d32 = 2.0df;
  d64 = 2.0dd;
  d128 = 2.0dl;

  /* To generic floating types.  */
  if ((float) d32 != 2.0f)
    abort ();
  if ((double) d32 != 2.0l)
    abort ();
  if ((float) d64 != 2.0f)
    abort ();
  if ((double) d64 != 2.0l)
    abort ();
  if ((float) d128 != 2.0f)
    abort ();
  if ((double) d128 != 2.0l)
    abort ();

  /* float to decimal floating types.  */
  if (d32 != (_Decimal32) f)
    abort ();
  if (d64 != (_Decimal64) f)
    abort ();
  if (d128 != (_Decimal128) f)
    abort ();

  /* double to decimal floating types.  */
  if (d32 != (_Decimal32) d)
    abort ();
  if (d64 != (_Decimal64) d)
    abort ();
  if (d128 != (_Decimal128) d)
    abort ();

  return 0;
}
