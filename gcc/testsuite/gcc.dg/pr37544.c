/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -msse2 -mtune=core2 -mfpmath=387" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-require-effective-target sse2 { target { i?86-*-* x86_64-*-* } } } */
/* { dg-require-effective-target sse2_runtime { target { i?86-*-* x86_64-*-* } } } */

extern void abort (void);

int main(void)
{
  double arr[1000];
  double a, b;

  int i;

  for (i = 0; i < 1000; i++)
    arr[i] = 4294967296.0 + (double)i;

  a = arr[0];
  b = (unsigned int)((unsigned long long int)a % 4294967296ULL);

  if (b >= 4294967296.0)
    abort ();

  return 0;
}
