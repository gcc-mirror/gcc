/* { dg-do compile } */
/* { dg-final { scan-assembler {\tvlbrh\t} } } */
/* { dg-final { scan-assembler {\tvlbrf\t} } } */
/* { dg-final { scan-assembler {\tvlbrg\t} } } */
/* { dg-final { scan-assembler-not {\tvperm\t} } } */

/* The addend X ensures that a LOAD REVERSE and not a STORE REVERSE is
   emitted.  */

void
vlbrh (unsigned short *a, unsigned short x)
{
  for (int i = 0; i < 128; ++i)
    a[i] = __builtin_bswap16 (a[i]) + x;
}

void
vlbrf (unsigned int *a, unsigned int x)
{
  for (int i = 0; i < 128; ++i)
    a[i] = __builtin_bswap32 (a[i]) + x;
}

void
vlbrg (unsigned long long *a, unsigned long long x)
{
  for (int i = 0; i < 128; ++i)
    a[i] = __builtin_bswap64 (a[i]) + x;
}
