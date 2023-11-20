/* { dg-do run } */
/* { dg-additional-options "-ftree-loop-distribution" } */

struct {
  int : 8;
  int a;
} b, d[4] = {{5}, {0}, {0}, {0}};
int c, e;
int main() {
  for (c = 0; c < 2; c++)
    for (e = 0; e < 2; e++) {
      d[c + 1] = b = d[c];
      d[c].a = 0;
    }
  if (b.a != 0)
    __builtin_abort();
  return 0;
}
