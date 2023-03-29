/* { dg-do run } */
/* { dg-options "-O3" } */

int printf(const char *, ...);
int a[1] = {1};
short b, c = 5500;
int d;
long e;
char f = 1;
int main() {
  while (1) {
    long g = b < 1;
    e = g;
    break;
  }
  for (; f; f--) {
    if (e) {
      d = -(6L | -(c & 1000));
    }
    char h = d;
    if (b)
      b = 0;
    if (d < 200)
      while (1)
        printf("%d", a[c]);
    short i = h * 210;
    c = i;
  }
  return 0;
}

