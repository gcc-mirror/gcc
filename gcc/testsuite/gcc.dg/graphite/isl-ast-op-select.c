/* { dg-require-effective-target size32plus } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O2 -floop-nest-optimize" } */

static void kernel_gemm(int ni, int nj, int nk, double alpha, double beta, double C[1024][1024], double A[1024][1024], double B[1024][1024])
{
 int i, j, k;
 for (i = 0; i < ni; i++)
   for (j = 0; j < nj; j++)
     {
       C[i][j] *= beta;
       for (k = 0; k < nk; ++k)
         C[i][j] += alpha * A[i][k] * B[k][j];
     }
}

void *polybench_alloc_data (int, int);

int main(int argc, char** argv) {
  int ni = 1024;
  int nj = 1024;
  int nk = 1024;
  double alpha;
  double beta;
  double (*C)[1024][1024];
  C = (double(*)[1024][1024])polybench_alloc_data ((1024) * (1024), sizeof(double));
  double (*A)[1024][1024];
  A = (double(*)[1024][1024])polybench_alloc_data ((1024) * (1024), sizeof(double));
  double (*B)[1024][1024];
  kernel_gemm (ni, nj, nk, alpha, beta, *C, *A, *B);
}
