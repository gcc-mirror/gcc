// PR c++/84255
// { dg-do compile { target c++14 } }

template<typename T> constexpr int var;
template<typename T> constexpr int var = 1; // { dg-error "redefinition" }
