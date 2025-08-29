/* { dg-do compile } */
/* { dg-options "-mgfni -mavx512bw -mavx512f -O3 -march=x86-64 -mtune=generic" } */
/* { dg-final { scan-assembler-times "vgf2p8affineqb" 12 } } */

/* Based on a test case from Andrew Pinski */

#ifndef N
#define N 5
#endif

void
ubyteshiftl (unsigned char *restrict a, unsigned char *restrict b, unsigned char *restrict c, int len)
{
  int i;
  for (i = 0; i < len; i++)
    {
      a[i] = c[i] ? (a[i] | b[i]) << N : a[i];
      a[i] = (!c[i]) ? (a[i] ^ b[i]) << N : a[i];
    }
}

void
ubyteshiftr (unsigned char *restrict a, unsigned char *restrict b, unsigned char *restrict c, int len)
{
  int i;
  for (i = 0; i < len; i++)
    {
      a[i] = c[i] ? (a[i] | b[i]) >> N : a[i];
      a[i] = (!c[i]) ? (a[i] ^ b[i]) >> N : a[i];
    }
}

void
sbyteshiftl (signed char *restrict a, signed char *restrict b, signed char *restrict c, int len)
{
  int i;
  for (i = 0; i < len; i++)
    {
      a[i] = c[i] ? (a[i] | b[i]) << N : a[i];
      a[i] = (!c[i]) ? (a[i] ^ b[i]) << N : a[i];
    }
}

void
sbyteshiftr (signed char *restrict a, signed char *restrict b, signed char *restrict c, int len)
{
  int i;
  for (i = 0; i < len; i++)
    {
      a[i] = c[i] ? (a[i] | b[i]) >> N : a[i];
      a[i] = (!c[i]) ? (a[i] ^ b[i]) >> N : a[i];
    }
}

static inline unsigned char rol8(unsigned char v, int c)
{
  return (v >> c) | (v << (8-c));
}

static inline unsigned char ror8(unsigned char v, int c)
{
  return (v << c) | (v >> (8-c));
}

void
ubyterol (unsigned char *restrict a, unsigned char *restrict b, unsigned char *restrict c, int len)
{
  int i;
  for (i = 0; i < len; i++)
    {
      a[i] = c[i] ? rol8(a[i] | b[i], N) : a[i];
      a[i] = (!c[i]) ? rol8(a[i] ^ b[i], N) : a[i];
    }
}

void
ubyteror (unsigned char *restrict a, unsigned char *restrict b, unsigned char *restrict c, int len)
{
  int i;
  for (i = 0; i < len; i++)
    {
      a[i] = c[i] ? ror8(a[i] | b[i], N) : a[i];
      a[i] = (!c[i]) ? ror8(a[i] ^ b[i], N) : a[i];
    }
}
