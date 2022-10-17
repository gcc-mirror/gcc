// { dg-do compile { target c++11 } }
// { dg-additional-options -fconstexpr-fp-except }

constexpr double inf = 1./0.;
