// { dg-do compile { target c++11 } }
// { dg-options "-fconcepts" }

template<typename T>
struct Y {
  ~Y() requires(true) = default;
  ~Y() requires(false) {}
};
