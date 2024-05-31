// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }
template <class, class>
class NumericArray {};

template <class>
constexpr bool match_numeric_array = false;
template <class Scalar, class Shape>
constexpr bool
    match_numeric_array<NumericArray<Scalar, Shape>> =
        true;
template <class T>
concept cpt_NumericArrayContainer =
  match_numeric_array<T>;

template <class X>
concept cpt_NumericArray =
  requires{requires cpt_NumericArrayContainer<X>;};


template <class X>
requires (!cpt_NumericArray<X>) auto func(int, X) {}

template <class X>
requires (cpt_NumericArray<X>) auto func(int, X) {}

int main() {
  NumericArray<double, int> v5;
  func(0, v5);
}
