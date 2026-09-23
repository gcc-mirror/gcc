/* { dg-do compile } */
/* { dg-options "-O2 -fno-early-inlining" } */

typedef int blasint;
void blas_level1_thread(long);
void cblas_sscal(blasint, float, float, blasint incx) {
  blas_level1_thread(incx);
}
blasint c_api_check_saxpby_n;
float c_api_check_saxpby_beta;
long labs(long);
static void c_api_check_saxpby(blasint incy) {
  blasint incy_abs = labs(incy);
  cblas_sscal(c_api_check_saxpby_n, c_api_check_saxpby_beta, 0, incy_abs);
}
void __ctest_saxpby_c_api_inc_x_2_inc_y_1_N_100_run() {
  c_api_check_saxpby(1);
  c_api_check_saxpby(2);
}
