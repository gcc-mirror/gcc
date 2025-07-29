/* { dg-do compile } */

int a, b, c, d;
void e() {
  int *f = &b;
  for (a = 0; a < 8; a++) {
    *f = 0;
    for (c = 0; c < 2; c++)
      *f = *f == 0;
  }
}
int main() {
  e();
  int *g = &b;
  *g = *g == (d == 0);
  return 0;
}
