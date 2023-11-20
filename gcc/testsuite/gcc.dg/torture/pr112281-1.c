/* { dg-do run } */
/* { dg-additional-options "-ftree-loop-distribution" } */

struct {
  int : 8;
  int a;
} b, d[4] = {{0}, {0}, {0}, {5}};
int c, e;
int main() {
  for (c = 2; c; c--)
    for (e = 0; e < 2; e++) {
      d[c] = b = d[c + 1];
      d[c + 1].a = 0;
    }
  if (b.a != 0)
    __builtin_abort();
  return 0;
}
