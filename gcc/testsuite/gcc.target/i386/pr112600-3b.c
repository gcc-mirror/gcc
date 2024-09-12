/* PR middle-end/112600 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse2" } */

#define MIN -32768
#define MAX 32767

typedef short T;
typedef unsigned short UT;

void foo (T *out, T *op_1, T *op_2, int n)
{
  int i;

  for (i = 0; i < n; i++)
    {
      T x = op_1[i];
      T y = op_2[i];
      T sum = (UT) x + (UT) y;

      out[i] = (x ^ y) < 0 ? sum : (sum ^ x) >= 0 ? sum : x < 0 ? MIN : MAX;
    }
}

/* { dg-final { scan-assembler "paddsw" } } */
