// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template<typename> concept conc = requires { true; };
constexpr auto r = ^^conc;
constexpr auto x = ^^conc<int>;  // { dg-error "cannot be applied to a concept check" }
