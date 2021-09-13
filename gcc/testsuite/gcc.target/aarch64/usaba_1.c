/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-reassoc" } */

#pragma GCC target "+nosve"

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))

#define FUNC(T, N, S)	\
void saba_##S (T * __restrict__ a, T * __restrict__ b, T * __restrict__ c)	\
{	\
  int i;	\
  for (i = 0; i < N; i++)	\
    c[i] += (MAX (a[i], b[i]) - MIN (a[i], b[i]));	\
}

FUNC (signed char, 16, qi)
/* { dg-final { scan-assembler-times {saba\tv[0-9]+\.16b, v[0-9]+\.16b, v[0-9]+\.16b} 1 } } */
FUNC (short, 8, hi)
/* { dg-final { scan-assembler-times {saba\tv[0-9]+\.8h, v[0-9]+\.8h, v[0-9]+\.8h} 1 } } */
FUNC (int, 4, si)
/* { dg-final { scan-assembler-times {saba\tv[0-9]+\.4s, v[0-9]+\.4s, v[0-9]+\.4s} 1 } } */
FUNC (unsigned char, 16, uqi)
/* { dg-final { scan-assembler-times {uaba\tv[0-9]+\.16b, v[0-9]+\.16b, v[0-9]+.16b} 1 } } */
FUNC (unsigned short, 8, uhi)
/* { dg-final { scan-assembler-times {uaba\tv[0-9]+\.8h, v[0-9]+\.8h, v[0-9]+\.8h} 1 } } */
FUNC (unsigned int, 4, usi)
/* { dg-final { scan-assembler-times {uaba\tv[0-9]+\.4s, v[0-9]+\.4s, v[0-9]+\.4s} 1 } } */

