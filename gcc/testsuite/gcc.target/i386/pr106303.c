/* { dg-do compile } */
/* { dg-options "-O2 -fno-inline-small-functions" } */

struct a {
  int b;
  int c;
  int d;
  int e;
} i, j;
int f, g, h;
struct a k() {
  while (f)
    i = j;
  if (g) {
    for (; h; h++)
      i = j;
    return j;
  }
  return i;
}
int main() {
  k();
  return 0;
}

