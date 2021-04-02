/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-additional-options "-w -O3 -march=armv8.3-a" } */

class a {
  float b;
  float c;

public:
  a(float d, float e) : b(d), c(e) {}
  a operator+(a d) { return a(b + d.b, c + d.c); }
  a operator-(a d) { return a(b - d.b, c - d.c); }
  a operator*(a d) { return a(b * b - c * c, b * c + c * d.b); }
};
long f;
a *g;
class {
  a *h;
  long i;
  a *j;

public:
  void k() {
    a l = h[0], m = g[i], n = l * g[1], o = l * j[8];
    g[i] = m + n;
    g[i + 1] = m - n;
    j[f] = o;
  }
} p;
main() { p.k(); }
