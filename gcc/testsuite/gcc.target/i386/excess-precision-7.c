/* Excess precision tests.  Test C99 semantics for conversions from
   integers to floating point: no excess precision for either explicit
   or implicit conversions.  */
/* { dg-do run } */
/* { dg-options "-std=c99 -mfpmath=387 -fexcess-precision=standard" } */

#ifdef __cplusplus
extern "C" {
#endif
extern void abort (void);
extern void exit (int);
#ifdef __cplusplus
}
#endif

int
main (void)
{
  float f = 1.0f;
  int i;

  i = 0x10001234;
  if ((float) i != 0x10001240)
    abort ();

  i = 0x10001234;
  i += f;
  if (i != 0x10001241)
    abort ();

  i = 0x10001234;
  i += 1.0f;
  if (i != 0x10001241)
    abort ();

  i = 0x10001234;
  i = i + f;
  if (i != 0x10001241)
    abort ();

  i = 0x10001234;
  i = i + 1.0f;
  if (i != 0x10001241)
    abort ();

  exit (0);
}
