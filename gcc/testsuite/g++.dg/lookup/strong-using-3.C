// PR c++/13659

// { dg-do compile }

namespace bar {
  inline namespace foo {
    template <class T> void f(T, T);
  }
  template <class T> void f(T);
}

int main() {
  // Make sure both declarations are brought in.
  using bar::f;
  f(1);
  f(1, 1);
}
