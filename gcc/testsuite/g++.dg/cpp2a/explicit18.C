// PR c++/100065
// { dg-do compile { target c++20 } }

template<bool B>
struct bool_constant {
  static constexpr bool value = B;
  constexpr operator bool() const { return value; }
};

using true_type = bool_constant<true>;
using false_type = bool_constant<false>;

template<bool>
struct X {
    template<typename T>
    X(T);
};

template<bool b>
explicit(b) X(bool_constant<b>) -> X<b>;

X false_ = false_type{}; // OK
X true_  = true_type{};  // { dg-error "explicit deduction guide" }
