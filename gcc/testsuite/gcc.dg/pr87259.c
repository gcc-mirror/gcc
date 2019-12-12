/* { dg-do compile } */
/* { dg-options "-Ofast" } */

int a, b, c;
int *e;
float f;
void h() {
  for (int g;;) {
    float d = b, i = 0 / f, j = a / (f * f), k, l = 0 / d;
    c = i + j;
    g = l;
    e[g] = c / d * k / d;
  }
}
