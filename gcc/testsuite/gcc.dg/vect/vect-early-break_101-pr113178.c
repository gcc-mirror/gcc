/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

struct PixelWeight {
  int m_SrcStart;
  int m_Weights[16];
};
char h;
void f(struct PixelWeight *pPixelWeights) {
    int dest_g_m;
    long tt;
    for (int j = 0; j < 16; j++) {
      int *p = 0;
      if (j < pPixelWeights->m_SrcStart)
        p = tt ? &pPixelWeights->m_Weights[0] : 0;
      int pWeight = *p;
      dest_g_m += pWeight;
    }
    h = dest_g_m;
}
