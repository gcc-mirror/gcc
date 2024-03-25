/* { dg-do compile } */
/* { dg-options "-O2 -mlsx -fno-vect-cost-model" } */
/* { dg-final { scan-assembler-times "vrotr\\.w" 2 } } */
/* { dg-final { scan-assembler-times "vneg\\.w" 1 } } */

#ifndef VLEN
#define VLEN 16
#endif

#ifndef TYPE
#define TYPE int
#endif

typedef unsigned TYPE V __attribute__ ((vector_size (VLEN)));
V a, b, c;

void
test (int x)
{
  b = a << x | a >> ((int)sizeof (TYPE) * __CHAR_BIT__ - x);
}

void
test2 (void)
{
  for (int i = 0; i < VLEN / sizeof (TYPE); i++)
    c[i] = a[i] << b[i] | a[i] >> ((int)sizeof (TYPE) * __CHAR_BIT__ - b[i]);
}
