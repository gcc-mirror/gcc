/* { dg-do run } */
/* { dg-additional-options "-ffinite-loops" } */

int a, b, c;
int d() {
  while (a)
    ;
  return 0;
}
static int e(int f, int g) {
  c = f;
  while (1) {
    f = 0;
    do {
      c = -c;
      if (c >= 0)
        goto h;
    } while (-1 / b);
    return d();
  h:
    b = g;
  }
}
int main()
{
  while (e(-4, 3))
    ;
  return 0;
}
