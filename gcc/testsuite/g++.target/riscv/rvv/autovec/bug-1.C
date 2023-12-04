/* { dg-options "-march=rv64gcv_zvl512b -mabi=lp64d -O3" } */

class c {
public:
  int e();
  void j();
};

float *d;
class k {
  int f;

public:
  k(int m) : f(m) {}
  float g;
  float h;
  void n(int m) {
    for (int i; i < m; i++) {
      d[0] = d[1] = d[2] = g;
      d[3] = h;
      d += f;
    }
  }
};

c l;
void o() {
  int b = l.e();
  k a(b);
  for (;;)
    if (b == 4) {
      l.j();
      a.n(2);
    }
}
