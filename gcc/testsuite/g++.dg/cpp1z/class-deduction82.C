// PR c++/99009
// { dg-do compile { target c++17 } }

template<typename> struct B {
  B(int = A()) {}
  template <typename ...> struct A;
};

template<typename T> struct X {
  template <T...> struct Y;
  X() { Y y; };
};
