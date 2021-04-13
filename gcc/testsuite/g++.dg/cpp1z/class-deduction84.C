// PR c++/99008
// { dg-do compile { target c++17 } }

template <class> struct A { A(int = 0); };
template <class = void> using B = A<int>;
auto x = B{};  // { dg-error "alias template deduction only available with" "" { target c++17_only } }
auto y = B();  // { dg-error "alias template deduction only available with" "" { target c++17_only } }
auto z = B{1}; // { dg-error "alias template deduction only available with" "" { target c++17_only } }
auto w = B(1); // { dg-error "alias template deduction only available with" "" { target c++17_only } }
