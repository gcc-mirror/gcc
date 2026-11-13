/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+dotprod -mtune=ampere1 -fopt-info-vec -fdump-tree-vect-details" } */
/* { dg-prune-output "epilogue loop vectorized" } */

/* ampere1 tuning unrolls dot, sad, sum and count into four dot-product
   accumulators, a vectorization factor of 64 with 16-byte vectors.
   sad_nodot and sum16 are not dot products and keep VF 16 and 32.  */

int __attribute__((noipa))
dot (const signed char *a, const signed char *b, int n)
{
  int s = 0;
  for (int i = 0; i < n; i++) /* { dg-optimized "16 byte vectors and unroll factor 64" } */
    s += a[i] * b[i];
  return s;
}

int __attribute__((noipa))
sad (const unsigned char *a, const unsigned char *b, int n)
{
  int s = 0;
  for (int i = 0; i < n; i++) /* { dg-optimized "16 byte vectors and unroll factor 64" } */
    s += __builtin_abs (a[i] - b[i]);
  return s;
}

int __attribute__((noipa, target ("+nodotprod")))
sad_nodot (const unsigned char *a, const unsigned char *b, int n)
{
  int s = 0;
  for (int i = 0; i < n; i++) /* { dg-optimized "16 byte vectors and unroll factor 16" } */
    s += __builtin_abs (a[i] - b[i]);
  return s;
}

int __attribute__((noipa))
sum (const unsigned char *a, int n)
{
  int s = 0;
  for (int i = 0; i < n; i++) /* { dg-optimized "16 byte vectors and unroll factor 64" } */
    s += a[i];
  return s;
}

unsigned short __attribute__((noipa))
sum16 (const unsigned char *a, int n)
{
  unsigned short s = 0;
  for (int i = 0; i < n; i++) /* { dg-optimized "16 byte vectors and unroll factor 32" } */
    s += a[i];
  return s;
}

int __attribute__((noipa))
count (const _Bool *b, int n)
{
  int s = 0;
  for (int i = 0; i < n; i++) /* { dg-optimized "16 byte vectors and unroll factor 64" } */
    s += b[i];
  return s;
}

/* { dg-final { scan-tree-dump-times "Choosing vector mode V16QI" 6 "vect" } } */
