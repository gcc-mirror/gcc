// PR c++/99507
// { dg-do compile { target c++20 } }

constexpr int i{0};
consteval const int &iref () { return i; }
const int *a{&iref ()};
