// { dg-do compile { target c++11 } }
// { dg-options "-fconcepts" }

template<typename T>
struct Y {
  ~Y() requires(true) = default;
  ~Y() requires(false) {}
};

Y<int> y;

template<typename T>
struct X {
  ~X() requires(sizeof(T) == 8) = default;
  ~X() requires(sizeof(T) != 8) {}
};

X<int> x;
