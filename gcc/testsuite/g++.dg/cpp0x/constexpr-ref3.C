// PR c++/50298
// { dg-do compile { target c++11 } }

int global_variable;

template <class T> struct X {
  static constexpr T r = global_variable;
};

X<int&> x;
