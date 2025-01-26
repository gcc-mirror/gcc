/* { dg-additional-options "-march=rv64gv -mabi=lp64" { target { rv64 } } } */

float b[2], c[2];
void d();
int h1();
void e(float * __restrict h) {
  int f;
  for (int f = 0; f < 4; f++) {
    if (h1())
      d();
  }
  for (int g = 0; g < 4; g++) {
    c[0] = h[0] - b[0];
    c[1] = h[1] - b[1];
    d();
    h += 1;
  }
}
