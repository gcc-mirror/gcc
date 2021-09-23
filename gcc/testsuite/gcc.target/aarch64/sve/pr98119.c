/* { dg-options "-O3 -msve-vector-bits=512 -mtune=thunderx" } */

void
f (unsigned short *x)
{
  for (int i = 0; i < 1000; ++i)
    x[i] += x[i - 16];
}

/* { dg-final { scan-assembler-not {\tubfx\t[wx][0-9]+, [wx][0-9]+, #?1, #?5\n} } } */
/* { dg-final { scan-assembler-not {\tand\tx[0-9]+, x[0-9]+, #?-63\n} } } */
/* { dg-final { scan-assembler {\tubfx\t[wx][0-9]+, [wx][0-9]+, #?1, #?4\n} } } */
/* { dg-final { scan-assembler {\tand\tx[0-9]+, x[0-9]+, #?-31\n} } } */
