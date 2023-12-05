/* { dg-do compile } */

double *SVD_A_0;
int SVD_i, SVD_j, SVD_k, SVD_n;
double SVD_f;
void SVD() {
  SVD_i = SVD_n - 1;
  for (; SVD_i; SVD_i--) {
    for (; SVD_j; SVD_j++) {
      SVD_f = SVD_k = SVD_i;
      for (; SVD_k < SVD_n; SVD_k++)
        SVD_A_0[SVD_k] += SVD_f;
    }
    SVD_j = SVD_i;
    for (; SVD_j < SVD_n; SVD_j++)
      ;
  }
}
