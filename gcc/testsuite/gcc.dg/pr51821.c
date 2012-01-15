/* { dg-do run } */
/* { dg-options "-O2 -msse" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-require-effective-target sse_runtime { target { i?86-*-* x86_64-*-* } } } */

extern void abort (void);

unsigned int  __attribute__((noinline))
test (int shift_size)
{
  unsigned long long res = ~0;

  return res << shift_size;
}

int
main ()
{
  int dst = 32;

  if (test (dst) != 0)
    abort ();

  return 0;
}
