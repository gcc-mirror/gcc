/* { dg-do run } */
/* { dg-require-effective-target int32 } */
/* { dg-additional-options "-fno-tree-forwprop" } */

int a = 128, b, d;
long e = -2, c;
int main() {
  signed char f = a;
  int g = f;
  c = (g < 0) - e;
  unsigned char h = g;
  b = h % c;
  d = 9000000000000 << b;
  if (d > 1)
    __builtin_abort();
  return 0;
}
