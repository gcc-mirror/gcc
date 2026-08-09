/* Test that converting long double to float rounds only once.

   Alpha has no instruction for the conversion, so it goes through DFmode.
   Both values below sit just under the halfway point between two floats,
   so a single correct rounding gives the lower of the two.  Rounding to
   DFmode first can land exactly on that halfway point, and rounding again
   from there gives the upper one.  */

/* { dg-do run } */
/* { dg-options "-std=c99 -mieee" } */

extern void abort (void);

volatile long double a = 0x1.7ff802ffffffffffffffffffffp+13L;
volatile long double b = 0x1.7ff802fffffeffffffffffffffp+13L;

int
main (void)
{
  if ((float) a != 0x1.7ff802p+13f)
    abort ();
  if ((float) b != 0x1.7ff802p+13f)
    abort ();
  return 0;
}
