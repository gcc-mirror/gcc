/* Excess precision tests.  Test C11 semantics for conversions from
   integers to floating point: no excess precision for either explicit
   conversions, but excess precision for implicit conversions.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -mfpmath=387 -fexcess-precision=standard" } */

extern void abort (void);
extern void exit (int);

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
  if (i != 0x10001235)
    abort ();

  i = 0x10001234;
  i += 1.0f;
  if (i != 0x10001235)
    abort ();

  i = 0x10001234;
  i = i + f;
  if (i != 0x10001235)
    abort ();

  i = 0x10001234;
  i = i + 1.0f;
  if (i != 0x10001235)
    abort ();

  i = 0x10001235;
  i = (1 ? i : 1.0f);
  if (i != 0x10001235)
    abort ();

  i = 0x10001235;
  i = (1 ? i : f);
  if (i != 0x10001235)
    abort ();

  i = 0x10001235;
  i = (0 ? 1.0f :i);
  if (i != 0x10001235)
    abort ();

  i = 0x10001235;
  i = (0 ? f : i);
  if (i != 0x10001235)
    abort ();

  exit (0);
}
