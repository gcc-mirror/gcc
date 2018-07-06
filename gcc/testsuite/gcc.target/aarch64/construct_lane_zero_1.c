/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef long long v2di __attribute__ ((vector_size (16)));
typedef double v2df __attribute__ ((vector_size (16)));

v2di
construct_lanedi (long long *y)
{
  v2di x =
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  { 0, y[0] }
#else
  { y[0], 0 }
#endif
  ;
  return x;
}

v2df
construct_lanedf (double *y)
{
  v2df x =
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  { 0.0, y[0] }
#else
  { y[0], 0.0 }
#endif
  ;
  return x;
}

/* Check that creating V2DI and V2DF vectors from a lane with a zero
   makes use of the D-reg LDR rather than doing explicit lane inserts.  */

/* { dg-final { scan-assembler-times "ldr\td\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-not "ins\t" } } */
