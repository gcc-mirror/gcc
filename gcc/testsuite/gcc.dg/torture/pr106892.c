/* { dg-do run } */

int a, b, c, d, e;
int f[8];
static int g() {
  while (a)
    a >>= 4;
  return 0;
}
static int h(int i) {
  if (i >= '0')
    return i - '0';
  //__builtin_unreachable ();
}
void __attribute__((noipa)) j(int i) {
  for (b = 2; g() <= 7; b++)
    if (i) {
      for (; e <= 7; e++)
        for (c = 1; c <= 7; c++) {
          d = h(b + '0');
          f[-d + 4] ^= 3;
        }
      return;
    }
}
int main() {
  j(1);
  if (f[2] != 0)
    __builtin_abort ();
}
