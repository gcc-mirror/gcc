/* { dg-do compile } */
/* { dg-options "-O2 -march=znver3" } */

extern "C" double pow(double, double);
double sigmoid_x;
double sigmoid() {
  if (sigmoid_x)
    return sigmoid_x;
  return 0;
}
double der_sigmoid() {
  double tmp = sigmoid();
  return tmp - pow(tmp, 2);
}

