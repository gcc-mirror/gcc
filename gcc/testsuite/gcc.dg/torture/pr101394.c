/* { dg-do compile } */

int a, b, c, d;
void h();
int e() __attribute__((returns_twice));
void f() {
  int *g = (int *)(__INTPTR_TYPE__)c;
  if (b) {
    h();
    g--;
    if (a)
      if (d)
        h();
  }
  if (g++)
    e();
  c = (__INTPTR_TYPE__)g;
}
