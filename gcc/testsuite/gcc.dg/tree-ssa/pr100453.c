/* { dg-do run } */
/* { dg-options "-O1" } */

struct a {
  int b : 4;
} d;
static int c, e;
static const struct a f;
static void g(const struct a h) {
  for (; c < 1; c++)
    d = h;
  e = h.b;
  c = h.b;
}
int main() {
  g(f);
  return 0;
}
