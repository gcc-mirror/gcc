/* PR middle-end/112600 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */

#define MIN -128
#define MAX 127

typedef char T;
typedef unsigned char UT;

void foo (T *out, T *op_1, T *op_2, int n)
{
  int i;

  for (i = 0; i < n; i++)
    {
      T x = op_1[i];
      T y = op_2[i];
      T dif = (UT) x - (UT) y;

      out[i] = (x ^ y) >= 0 ? dif : (dif ^ x) >= 0 ? dif : x < 0 ? MIN : MAX;
    }
}

/* { dg-final { scan-assembler "psubsb" } } */
