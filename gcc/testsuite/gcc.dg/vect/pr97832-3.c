/* { dg-do compile } */
/* { dg-additional-options "-Ofast" } */
/* { dg-require-effective-target vect_double } */

void foo(double* restrict y, const double* restrict x0, const double* restrict x1, int clen)
{
  int xi = clen & 2;
  double f00_re = x0[0+xi+0];
  double f10_re = x1[0+xi+0];
  double f01_re = x0[0+xi+1];
  double f11_re = x1[0+xi+1];
  double f00_im = x0[4+xi+0];
  double f10_im = x1[4+xi+0];
  double f01_im = x0[4+xi+1];
  double f11_im = x1[4+xi+1];
  int clen2 = (clen+xi) * 2;
  double* y0 = &y[0];
  double* y1 = &y[clen2];
  #pragma GCC unroll 0
  for (int c = 0; c < clen2; c += 8) {
    // y0[c] = y0[c] - x0[c]*conj(f00) - x1[c]*conj(f10);
    // y1[c] = y1[c] - x0[c]*conj(f01) - x1[c]*conj(f11);
    #pragma GCC unroll 4
    for (int k = 0; k < 4; ++k) {
      double x0_re = x0[c+0+k];
      double x0_im = x0[c+4+k];
      double y0_re = y0[c+0+k];
      double y0_im = y0[c+4+k];
      double y1_re = y1[c+0+k];
      double y1_im = y1[c+4+k];
      y0_re = y0_re - x0_re * f00_re - x0_im * f00_im;
      y0_im = y0_im + x0_re * f00_im - x0_im * f00_re;
      y1_re = y1_re - x0_re * f01_re - x0_im * f01_im;
      y1_im = y1_im + x0_re * f01_im - x0_im * f01_re;
      double x1_re = x1[c+0+k];
      double x1_im = x1[c+4+k];
      y0_re = y0_re - x1_re * f10_re - x1_im * f10_im;
      y0_im = y0_im + x1_re * f10_im - x1_im * f10_re;
      y1_re = y1_re - x1_re * f11_re - x1_im * f11_im;
      y1_im = y1_im + x1_re * f11_im - x1_im * f11_re;
      y0[c+0+k] = y0_re;
      y0[c+4+k] = y0_im;
      y1[c+0+k] = y1_re;
      y1[c+4+k] = y1_im;
    }
  }
}

/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { target { ! { vect_load_lanes && vect_strided8 } } } } } */
/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" { target { ! { vect_load_lanes && vect_strided8 } } } } } */
