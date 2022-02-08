// PR c++/103706
// { dg-do compile { target c++20 } }

template<class T> concept C = __is_same(T, int);

template<class... Ts> void f() {
  ([]() requires C<Ts> { return Ts(); }(), ...); // { dg-error "no match" }
}

template void f<int, int, int>(); // { dg-bogus "" }
template void f<int, int, char>(); // { dg-message "required from here" }
