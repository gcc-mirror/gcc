/* { dg-do run } */

long a, b, *c = &b;
short d, e;
int main() {
  int f = 0;
  for (; f != 1; f = (short)(f - 1)) {
    d = -f;
    e = a && e;
    *c = 0 > f;
  }
  if (b != 0)
    __builtin_abort();
  return 0;
}
