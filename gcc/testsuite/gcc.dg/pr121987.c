/* { dg-do compile } */
/* { dg-options "-O3" } */
int a, b, c, d;
int main() {
  unsigned long e = 10000000000;
  unsigned f;
  int g;
  while (a) {
    c = 1;
    d = f;
    f = ~(~(~(b && g) % a * ~e) << c);
    b = e && f % e + ~f;
    g = a;
  }
  return 0;
}
