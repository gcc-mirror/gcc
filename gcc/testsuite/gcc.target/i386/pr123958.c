/* { dg-do compile } */
/* { dg-options "-O3 -march=znver3" } */

float fnk00xX_rae1_1;
double pow(double, double);
double sqrt(double);
void write_r4(float *);
void fnk00xX() {
  int i;
  float rs2;
  double ds2;
  for (; i; i += 1) {
    sqrt(i);
    ds2 = ds2 + pow(fnk00xX_rae1_1, 2.0);
    rs2 = ds2;
  }
  write_r4(&rs2);
}
