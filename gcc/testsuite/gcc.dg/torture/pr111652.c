/* { dg-do run } */
/* { dg-additional-options "-fsplit-loops" } */

volatile int a;
int b;
int main() {
  for (; b < 5; b += 3) {
    b && a;
    if (b < 4)
      a--;
  }
  if (b != 6)
    __builtin_abort();
  return 0;
}
