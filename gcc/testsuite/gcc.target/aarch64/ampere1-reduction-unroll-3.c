/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+dotprod -mtune=ampere1 -fopt-info-vec -fdump-tree-vect-details" } */

/* These loops run fewer than six 16-byte vector iterations, so ampere1
   tuning does not unroll them into more dot-product accumulators.  */

int __attribute__((noipa))
dot64 (const signed char *a, const signed char *b)
{
  int s = 0;
  for (int i = 0; i < 64; i++) /* { dg-optimized "16 byte vectors and unroll factor 16" } */
    s += a[i] * b[i];
  return s;
}

int __attribute__((noipa))
sad80 (const unsigned char *a, const unsigned char *b)
{
  int s = 0;
  for (int i = 0; i < 80; i++) /* { dg-optimized "16 byte vectors and unroll factor 16" } */
    s += __builtin_abs (a[i] - b[i]);
  return s;
}

/* { dg-final { scan-tree-dump-times "Choosing vector mode V16QI" 2 "vect" } } */
