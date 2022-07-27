// PR c++/104641
// { dg-do compile { target c++11 } }

template<class T>
struct A {
  template<class U> struct B { B(U); };
private:
  template<class U> struct C { C(U); };
};

template<class T>
void f() {
  typename A<T>::B x = 0;
  auto y = typename A<T>::B(0);
  auto z = typename A<T>::B{0};
  typename A<T>::C w(0); // { dg-error "private" "" { target c++17 } }
}

template void f<void>();

// { dg-error "not a type" "" { target c++14_down } 13 }
// { dg-error "not a type" "" { target c++14_down } 14 }
// { dg-error "not a type" "" { target c++14_down } 15 }
// { dg-error "not a type" "" { target c++14_down } 16 }
