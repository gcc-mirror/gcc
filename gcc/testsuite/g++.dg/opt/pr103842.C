// PR target/103842
// { dg-do compile }
// { dg-options "-O3 -std=c++14" }

void foo (float *);
struct M {
  float x[3][3];
  float *operator[](int i) { return x[i]; }
  M();
  M(float f, float g) {
    x[1][0] = x[1][1] = x[1][2] = f;
    x[2][0] = g;
  }
  void bar();
  M baz() {
    M s(x[1][2] - x[1][2], x[1][1] - x[1][1]);
    float r = s[2][0];
    if (r)
      for (int i = 0; i < 3; ++i)
	for (int j = 0; j < 3; ++j)
	  s[i][j] /= r;
    for (int i = 0;;) {
      float *t = s[i];
      foo(t);
    }
  }
};
void qux() {
  M m, i = m.baz(), j = i;
  j.bar();
}
