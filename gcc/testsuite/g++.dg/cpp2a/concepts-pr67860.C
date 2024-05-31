// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

#include <type_traits>

inline constexpr bool and_impl() { return true; }

template <class OperandFirst, class... OperandsRest>
constexpr bool and_impl(OperandFirst operand_first,
                        OperandsRest... operands_rest) {
  return operand_first && and_impl(operands_rest...);
}

template <class... Operands> constexpr bool and_(Operands... operands) {
  return and_impl(operands...);
}

template <class X> concept C = true;

// v1
template<int, class... Xs>
  requires (and_(C<Xs>...))
constexpr int f(const Xs&... xs) {
  return 0;
}

// v2
template<int, class... Xs>
constexpr int f(const Xs&... xs) {
  return 1;
}

int main() {
  static_assert(f<10>(3.0, 2.0f) == 0);
  return 0;
}

// 2nd example

template <typename T, typename... Us>
concept AreType =
  (std::is_same<T,Us>::value && ...);
  // return true; gives the same overloaded error


// Function with constraint
template<typename T, AreType<T>... Us>
constexpr bool isValid(Us... us) {
  return true;
}

// Function with no constraint
template<typename T, typename... U>
constexpr bool isValid(U... u) {
  return false;
}

int main2() {
  static_assert(isValid<int>(1)); // also isValid<int>(1, 2, 3); etc
  return 0;
}
