// PR c++/89357
// { dg-do compile { target c++11 } }

void g(int &);

void f0() {
  __attribute__((aligned(128))) static int x;
  g(x);
}

void f1() {
  alignas(128) int x;
  g(x);
}
