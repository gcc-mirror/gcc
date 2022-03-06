// PR c++/103706
// { dg-do compile { target c++20 } }

template<class T, class U> concept C = __is_same(U, int);

template<class T> void f() {
  []() -> C<T> auto {
    C<T> auto x = T(); // { dg-error "constraints" }
    return T(); // { dg-error "constraints" }
  }();
}

template void f<int>(); // { dg-bogus "" }
template void f<char>(); // { dg-message "required from here" }
