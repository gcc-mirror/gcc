/* { dg-do compile } */
/* { dg-options "-O2 -g -w" } */

int a, b, c = 7, d;
static unsigned e, g;
char f;
static unsigned fn1() {
  unsigned h = e - b ^ c;
  int i = h / c & a * g, j = g * h;
  if (h) {
    if (d)
      h = e;
    j = a;
    a = (a && (g % f && i) % h) | c | ~2;
    if (b)
      printf("", 1);
  }
  c = i;
  a = j;
  return 2;
}

int main() {
  for (; b < -18; --b)
    g = 0;
  fn1();
  return 0;
}
