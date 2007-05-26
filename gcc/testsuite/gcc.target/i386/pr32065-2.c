/* { dg-do run { target dfp } } */
/* { dg-options "-Os -msse -std=gnu99" } */

extern void abort (void);

int main()
{
  if (7.999999999999999999999999999999999E6144dl + 3.0E6144dl
      != __builtin_infd32 ())
    abort ();

  return 0;
}
