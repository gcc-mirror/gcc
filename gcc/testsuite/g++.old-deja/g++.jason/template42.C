// { dg-do run  }
// Testcase for not evaluating template default args if they are
// never used.

struct X {
  X(int) { }
};

template <class T>
struct A {
  void f (T t = T()) { }
};

int main ()
{
  A<X> a;
  X x (1);
  a.f (x);
}
