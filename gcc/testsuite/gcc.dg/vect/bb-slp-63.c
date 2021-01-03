/* { dg-do compile } */

struct {
  unsigned a;
  unsigned c;
} d;
int e, g;
void h(unsigned b) {
  unsigned a, c;
  while (e) {
    if (b) {
      ++e;
      continue;
    }
    c = g;
    if (g)
      a |= 10;
  }
  d.a = a;
  d.c = c;
}
