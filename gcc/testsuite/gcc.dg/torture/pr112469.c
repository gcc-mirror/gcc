/* { dg-do compile } */

int a, b, c;
static int *d = &a;
int e(int f) { return f == 0 ? 1 : f; }
void g() {
  a = 1;
  for (; a <= 8; a++) {
    b = e(*d);
    c = -b;
  }
}
