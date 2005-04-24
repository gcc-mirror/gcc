/* { dg-do compile } */
/* { dg-options "-O2 -funroll-loops -march=nocona" } */

#include <mmintrin.h>

int W;
void f()
{
  int j;
  int B, C;
  unsigned char* S;
  __m64 *T = (__m64 *) &W;

  for (j = 0; j < 16; j++, T++)
  {
    T[0] = T[1] = _mm_set1_pi8(*S);
    S += W;
  }

  C = 3 * B;

  __m64 E = _mm_set_pi16(3 * B, 3 * B, 3 * B, 5 * B);
  __m64 G = _mm_set1_pi16(3 * B);

  for (j = 0; j < 16; j++)
  {
    __m64 R = _mm_set1_pi16(B + j * C);
    R = _m_paddw(R, E);
    R = _m_paddw(R, G);
    T[0] = _mm_srai_pi16(R, 3);
  }
}
