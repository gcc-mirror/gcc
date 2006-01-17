/* { dg-options "-std=gnu99" } */

/* N1150 5.1 Conversion between decimal floating integer.
   C99 6.3.1.4(1a) New.
   Test integer saturation.  */

#include <decfloat.h>
#include <limits.h>

extern void abort (void);

volatile _Decimal32 d32;
volatile _Decimal64 d64;
volatile _Decimal128 d128;

volatile signed int si;
volatile unsigned int usi;
volatile unsigned long long udi;

int
main ()
{

  /* Unsigned.  */
  usi = DEC32_MAX;  /* { dg-warning "overflow in implicit constant conversion" } */
  if (usi != UINT_MAX)
    abort ();

  usi = DEC64_MAX;  /* { dg-warning "overflow in implicit constant conversion" } */
  if (usi != UINT_MAX)
    abort ();

  usi = DEC128_MAX; /* { dg-warning "overflow in implicit constant conversion" } */
  if (usi != UINT_MAX)
    abort ();

  /* Signed.  */
  si = DEC32_MAX;	/* { dg-warning "overflow in implicit constant conversion" } */
  if (si != INT_MAX)
    abort ();

  si = DEC64_MAX;   /* { dg-warning "overflow in implicit constant conversion" } */
  if (si != INT_MAX)
    abort ();

  si = DEC128_MAX;  /* { dg-warning "overflow in implicit constant conversion" } */
  if (si != INT_MAX)
    abort ();

  si = - DEC32_MAX; /* { dg-warning "overflow in implicit constant conversion" } */
  if (si != INT_MIN)
    abort ();

  si = - DEC64_MAX; /* { dg-warning "overflow in implicit constant conversion" } */
  if (si != INT_MIN)
    abort ();

  si = - DEC128_MAX; /* { dg-warning "overflow in implicit constant conversion" } */
  if (si != INT_MIN)
    abort ();

  return 0;
}
