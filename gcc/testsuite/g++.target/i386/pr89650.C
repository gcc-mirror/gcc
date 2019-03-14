// { dg-do compile { target c++11 } }
// { dg-options "-O2 -flive-range-shrinkage -fno-tree-dce -fno-dce -fnon-call-exceptions -mavx" }

int d, e;
struct g {
  float f;
  g(float h) : f(h + d) {}
  ~g() {}
};
struct i {
  int a;
  int b : 4;
  int &c;
  i(int h) : a(), b(), c(h) {}
};
int main() {
  i j(e);
  g k[]{1, 2};
}
