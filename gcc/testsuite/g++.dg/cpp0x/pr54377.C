// PR c++/54377
// { dg-do compile { target c++11 } }

template <typename, typename, typename = void, typename...>
struct foo {};  // { dg-message "provided for" }

foo<int> f;     // { dg-error "at least 2" }
