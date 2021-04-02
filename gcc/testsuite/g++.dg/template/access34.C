// PR c++/58993
// { dg-do compile }

class base { void foo(); };

template <class T>
struct bar : public base {
  void f1() {
    &base::foo;  // { dg-error "private" }
  }

  template <class>
  void f2() {
    &base::foo;  // { dg-error "private" }
  }

  void f3();
};

template <class T>
void bar<T>::f3() {
  (void) &base::foo; // { dg-error "private" }
}

int main() {
  bar<int>().f1();
  bar<int>().f2<int>();
  bar<int>().f3();
}
