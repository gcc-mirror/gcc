/* Excess precision tests.  Test implicit conversions in comparisons:
   no excess precision in C99 mode.  */
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
  float f = 0x1p63f;
  unsigned long long int u = (1ULL << 63) + 1;

  if ((f == u) != 1)
    abort ();

  if ((u == f) != 1)
    abort ();

  if ((f != u) != 0)
    abort ();

  if ((u != f) != 0)
    abort ();

  if ((f < u) != 0)
    abort ();

  if ((u < f) != 0)
    abort ();

  if ((f <= u) != 1)
    abort ();

  if ((u <= f) != 1)
    abort ();

  if ((f > u) != 0)
    abort ();

  if ((u > f) != 0)
    abort ();

  if ((f >= u) != 1)
    abort ();

  if ((u >= f) != 1)
    abort ();

  exit (0);
}
