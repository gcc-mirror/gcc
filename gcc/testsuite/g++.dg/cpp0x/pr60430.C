// { dg-do compile { target c++11 } }

constexpr const int A = 42;
const int &B = A;
static_assert(&A == &B, "Bug");
