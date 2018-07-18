// PR c++/79361
// { dg-do compile { target c++11 } }

template<typename T> void foo(T);

template<> void foo<int>(int) {}   // { dg-message "declared" }
template<> void foo<int>(int) = delete;  // { dg-error "redefinition" }
