// PR c++/121443
// { dg-do compile { target c++11 } }

void f(int i = []() {
  struct X {
    int val;
    X(int v) : val(v) { }
  };
  X x(2);
  return x.val;
}()) {}
