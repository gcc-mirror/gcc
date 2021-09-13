/* { dg-do compile } */
/* { dg-options "-O1 -ftree-loop-vectorize -fvect-cost-model=unlimited" }  */

#define LTGT(a, b) (__builtin_islessgreater (a, b) ? a : b)
void foo (int ilast,float* w, float* w2)
{
  int i;
  for (i = 0; i < ilast; ++i)
  {
    w[i] = LTGT (0.0f, w2[i]);
  }
}
