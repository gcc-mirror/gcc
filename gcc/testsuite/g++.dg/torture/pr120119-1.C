// { dg-do compile }
// { dg-additional-options "-mcpu=cortex-a57" { target aarch64*-*-* } }

// PR target/120119

struct a {
  float operator()(int b, int c) { return d[c * 4 + b]; }
  float *d;
};
float e(float *);
auto f(a b) {
  float g[]{b(1, 1), b(2, 1), b(3, 1), b(1, 2), b(2, 2), b(3, 2), b(1, 3),
            b(2, 3), b(3, 3), b(3, 2), b(1, 3), b(2, 3), b(3, 3)};
  return b.d[0] * e(g);
}
