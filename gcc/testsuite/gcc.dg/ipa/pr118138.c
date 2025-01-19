/* { dg-do run } */
/* { dg-options "-O3 -fno-inline" } */

unsigned a = -1;
int b, e, c = 1;
unsigned long d;

long f(long g) {
  return g;
}

static long h(unsigned g) {
  for (; b < 8; b++)
    d = f(g);
  e = a < d;
  if (e)
    c = 0;
  return 0;
}

static void i(short g) {
  h(g);
}

int main() {
  i(-1);
  if (c != 1)
    __builtin_abort();
  return 0;
}
