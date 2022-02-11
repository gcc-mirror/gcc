/* { dg-do compile { target { aarch64-*-* } } } */
/* { dg-additional-options "-w -O3 -march=armv8.3-a -fdump-tree-slp-all" } */

class a {
  float b;
  float c;

public:
  a(float d, float e) : b(d), c(e) {}
  a operator+(a d) { return a(b + d.b, c + d.c); }
  a operator*(a d) { return a(b * b - c * c, b * c + c * d.b); }
};
int f, g;
class mp {
  a *h;
  a *i;

public:
  void j() {
    a k = h[0], l = i[g], m = k * i[f];
    i[g] = l + m;
    i[f] = m;
  }
} n;
main() { n.j(); }

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_MUL" 1 "slp2" { xfail { vect_float } } } } */
