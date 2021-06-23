/* { dg-do run } */

short a;
int b[5][4] = {2, 2};
int d;
short e(int f) { return f == 0 || a && f == 1 ? 0 : a; }
int main() {
  int g, h;
  g = 3;
  for (; g >= 0; g--) {
    h = 3;
    for (; h >= 0; h--)
      b[g][h] = b[0][1] && e(1);
  }
  d = b[0][1];
  if (d != 0)
    __builtin_abort ();
  return 0;
}
