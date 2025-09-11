/* { dg-do compile } */

int a, b;
short c;
short d(short e) { return e - 1; }
int main() {
  for (; c; c++)
    for (a = 2; a != 0; a = d(a)) {
      int *i = &b;
      *i &= a;
    }
  return 0;
}
