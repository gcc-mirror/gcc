// PR c++/79629
// { dg-do compile { target c++14 } }
// { dg-options "-w" }

template <typename> struct S {
  enum E : int;
  constexpr int g() const;
};
enum S<char>::E;
template <typename T> enum S<T>::E : int { b };
template <typename T>
constexpr int S<T>::g() const { b; } // { dg-error "not declared" }
static_assert(S<char>().g() == 1, ""); // { dg-error "" }
// { dg-message "in .constexpr. expansion of" "" { target *-*-* } .-1 }
