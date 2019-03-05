// P0892R2
// { dg-do compile }
// { dg-options "-std=c++2a" }

template<typename T>
struct B {
  static const T value = true;
};

struct X {
  template<typename T>
  explicit(B<T>::value) operator T();
};

int
main ()
{
  X x;
  int i = x.operator int();
  int i3 = x; // { dg-error "cannot convert" }
  int i2{x};
}
