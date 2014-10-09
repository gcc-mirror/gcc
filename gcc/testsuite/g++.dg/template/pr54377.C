// PR c++/54377

template <typename, typename, typename = void, typename = void>
struct foo {};  // { dg-message "provided for" }

foo<int> f;     // { dg-error "at least 2" }
