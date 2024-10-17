/* { dg-do compile } */
/* { dg-options "-O2 --param aarch64-autovec-preference=asimd-only" } */

#pragma GCC target "+nosve"


#define N 1024

unsigned short in_s[N];
unsigned short out_s[N];

void
foo16 (void)
{
  for (unsigned i = 0; i < N; i++)
  {
    unsigned short x = in_s[i];
    out_s[i] = (x >> 8) | (x << 8);
  }
}

/* { dg-final { scan-assembler {\trev16\tv([123])?[0-9]\.16b, v([123])?[0-9]\.16b} } } */

