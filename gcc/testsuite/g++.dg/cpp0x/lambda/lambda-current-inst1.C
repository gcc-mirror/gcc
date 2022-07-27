// PR c++/82980
// { dg-do compile { target c++11 } }

template <class T>
struct Outer
{
  template <class U>
  void f();

  void bar(Outer outer) {
    [outer](){ outer.f<int>(); };
  }
  void baz(Outer *p) {
    [&](){ p->f<int>(); };
  }
};

int main() { }
