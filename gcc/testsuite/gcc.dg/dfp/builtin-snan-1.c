/* Test __builtin_nansd* functions.  Test not requiring runtime
   exceptions support.  */
/* { dg-options "" } */

volatile _Decimal32 d32 = __builtin_nansd32 ("");
volatile _Decimal64 d64 = __builtin_nansd64 ("");
volatile _Decimal128 d128 = __builtin_nansd128 ("");

extern void abort (void);
extern void exit (int);

int
main (void)
{
  if (!__builtin_isnan (d32))
    abort ();
  if (!__builtin_isnan (d64))
    abort ();
  if (!__builtin_isnan (d128))
    abort ();
  exit (0);
}
