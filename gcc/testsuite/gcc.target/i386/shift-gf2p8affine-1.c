/* { dg-do compile } */
/* { dg-options "-mgfni -mavx512vl -mavx512bw -mavx512f -O3 -march=x86-64 -mtune=generic" } */
/* { dg-final { scan-assembler-times "vgf2p8affineqb" 14 } } */

#ifndef N
#define N 5
#endif

void
ubyteshiftl (unsigned char *a, int len)
{
  int i;
  for (i = 0; i < len; i++)
    a[i] <<= N;
}

void
ubyteshiftr (unsigned char *a, int len)
{
  int i;
  for (i = 0; i < len; i++)
    a[i] >>= N;
}

void
ubyteshiftl_mask (unsigned char *a, int len)
{
  int i;
  for (i = 0; i < len; i++)
    if (a[i] & 1)
      a[i] <<= N;
}

void
sbyteshiftl (signed char *a, int len)
{
  int i;
  for (i = 0; i < len; i++)
    a[i] <<= N;
}

void
sbyteshiftr (signed char *a, int len)
{
  int i;
  for (i = 0; i < len; i++)
    a[i] >>= N;
}

void
ubyteror (unsigned char *a, int len)
{
  int i;
  for (i = 0; i < len; i++)
    a[i] = a[i] << N | a[i] >> (8 - N);
}

void
ubyterol (unsigned char *a, int len)
{
  int i;
  for (i = 0; i < len; i++)
    a[i] = a[i] >> N | a[i] << (8 - N);
}
