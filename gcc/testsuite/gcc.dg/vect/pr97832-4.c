/* { dg-do compile } */
/* { dg-additional-options "-Ofast" } */
/* { dg-require-effective-target vect_double } */

void foo1x1(double* restrict y, const double* restrict x, int clen)
{
  int xi = clen & 2;
  double f_re = x[0+xi+0];
  double f_im = x[4+xi+0];
  int clen2 = (clen+xi) * 2;
#pragma GCC unroll 0
  for (int c = 0; c < clen2; c += 8) {
#pragma GCC unroll 4
    for (int k = 0; k < 4; ++k) {
      double x_re = x[k];
      double x_im = x[c+4+k];
      double y_re = y[c+0+k];
      double y_im = y[c+4+k];
      y_re = y_re - x_re * f_re - x_im * f_im;;
      y_im = y_im + x_re * f_im - x_im * f_re;
      y[c+0+k] = y_re;
      y[c+4+k] = y_im;
    }
  }
}

/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" } } */
/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" } } */
