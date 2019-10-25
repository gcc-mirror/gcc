/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -g" } */

typedef unsigned char u8;
typedef unsigned int u32;
typedef unsigned __int128 u128;

u32 b, c;

static inline
u128 bar (u8 d, u128 e)
{
  __builtin_memset (11 + (char *) &e, b, 1);
  d <<= e & 7;
  d = d | d > 0;
  return d + e;
}

void
foo (void)
{
  c = bar (~0, 5);
}
