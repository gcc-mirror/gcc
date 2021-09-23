/* { dg-do compile } */
/* { dg-additional-options "-Ofast" } */

typedef struct {
  double real;
  double imag;
} complex;
typedef struct {
  complex e[3][3];
} su3_matrix;
su3_matrix check_su3_c;
double check_su3_ar, check_su3_ari, check_su3_max;
int arireturn();
int check_su3() {
  check_su3_ar = check_su3_c.e[0][0].real * check_su3_c.e[1][0].real +
                 check_su3_c.e[0][0].imag * check_su3_c.e[1][0].imag +
                 check_su3_c.e[0][1].real * check_su3_c.e[1][1].real +
                 check_su3_c.e[0][1].imag * check_su3_c.e[1][1].imag +
                 check_su3_c.e[0][2].real * check_su3_c.e[1][2].real +
                 check_su3_c.e[0][2].imag * check_su3_c.e[1][2].imag;
  check_su3_max = check_su3_c.e[0][0].real * check_su3_c.e[2][0].real +
                  check_su3_c.e[0][0].imag * check_su3_c.e[2][0].imag +
                  check_su3_c.e[0][1].real * check_su3_c.e[2][1].real +
                  check_su3_c.e[0][1].imag * check_su3_c.e[2][1].imag +
                  check_su3_c.e[0][2].real * check_su3_c.e[2][2].real +
                  check_su3_c.e[0][2].imag * check_su3_c.e[2][2].imag;
  check_su3_ari = check_su3_ar;
  if (check_su3_ari)
    check_su3_max = check_su3_c.e[1][0].real * check_su3_c.e[2][0].real +
                    check_su3_c.e[1][0].imag * check_su3_c.e[2][0].imag +
                    check_su3_c.e[1][1].real * check_su3_c.e[2][1].real +
                    check_su3_c.e[1][1].imag * check_su3_c.e[2][1].imag +
                    check_su3_c.e[1][2].real * check_su3_c.e[2][2].real +
                    check_su3_c.e[1][2].imag * check_su3_c.e[2][2].imag;
  if (check_su3_max)
    arireturn();
  return 0;
}
