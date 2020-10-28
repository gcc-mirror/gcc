/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O1 -funsafe-math-optimizations -ftree-vectorize" }  */
/* { dg-add-options arm_neon } */

#define UNORD(a, b) (__builtin_isunordered (a, b) ? a : b)
void foo (int ilast,float* w, float* w2)
{
  int i;
  for (i = 0; i < ilast; ++i)
  {
    w[i] = UNORD (0.0f, w2[i]);
  }
}

/* { dg-final { scan-assembler "vcgt\\.f32\[\\t \]*q\[0-9\]+,\[\\t \]*q\[0-9\]+,\[\\t \]*#0" } } */
/* { dg-final { scan-assembler "vcle\\.f32\[\\t \]*q\[0-9\]+,\[\\t \]*q\[0-9\]+,\[\\t \]*#0" } } */
/* { dg-final { scan-assembler "vorr\[\\t \]*q\[0-9\]+,\[\\t \]*q\[0-9\]+,\[\\t \]*q\[0-9\]+" } } */
/* { dg-final { scan-assembler "vbsl|vbit|vbif\[\\t \]*q\[0-9\]+,\[\\t \]*q\[0-9\]+,\[\\t \]*q\[0-9\]+" } } */
