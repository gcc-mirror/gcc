/* { dg-do compile } */
/* { dg-options "-O3" } */
long a;
char b;
short c;
int f(int g) {
  {
    unsigned h = g;
    {
      long d = h;
      int e = 0;
      do {
        if ((h & 15) == 4)
          d = d + (h << 5);
        if (d == 5)
          break;
        e = e + 1;
      } while (e < 3);
      c = d;
    }
  }
  return c;
}
char k() {
  long i;
  unsigned j = b % 6u + 4;
  while (a)
    i = f(j + 826);
  return i;
}
int main() {}
