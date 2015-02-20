/* { dg-do run } */
/* { dg-options "-O2 -fno-inline" } */

extern void abort (void);

#define force_simd_si(v) asm volatile ("mov %s0, %1.s[0]" :"=w" (v) :"w" (v) :)

unsigned int
shft_add (unsigned int a, unsigned int b)
{
  unsigned int c;

  force_simd_si (a);
  force_simd_si (b);
  c = a >> b;
  force_simd_si (c);

  return c + b;
}

int
main (void)
{
  unsigned int i = 0;
  unsigned int a = 0xdeadbeef;

  for (i = 0; i < 32; i++)
  {
    unsigned int exp = (a / (1 << i) + i);
    unsigned int got = shft_add (a, i);

    if (exp != got)
      abort ();
  }

  return 0;
}

