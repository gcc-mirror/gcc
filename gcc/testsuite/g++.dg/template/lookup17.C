// PR c++/91706
// { dg-do compile { target c++11 } }
// { dg-additional-options -g }

template <bool> struct A;

struct B { static constexpr bool g = false; };

struct C {
  template <typename> static B c ();
};

template <class T> struct D : C {
  using c = decltype (c<T>());
  using E = A<c::g>;
};

D<int> g;
