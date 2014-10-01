// { dg-do compile { target c++11 } }

struct X {
  X() = default;
  template<class... U> X(U...);
};

struct Y {
  template<class... U> Y(U...);
};

#define SA(X) static_assert((X),#X)

SA(__is_trivially_constructible(X));
SA(!__is_trivially_constructible(Y));
