/* { dg-do compile } */

unsigned a, b;
int c, d;
unsigned e(int f) {
  if (a > f)
    return a;
  return f;
}
void g() {
  for (; c; c++)
    d = e(d);
  b = d;
}
